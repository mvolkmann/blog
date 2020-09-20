---
eleventyNavigation:
  key: Jupyter
  parent: Python
layout: topic-layout.njk
---

## Overview

From {% aTargetBlank "https://jupyter.org/", "jupyter.org" %},
"Project Jupyter exists to develop open-source software, open-standards,
and services for interactive computing across dozens of programming languages."

Jupyter is widely known for the "Jupyter Notebook" software.
It is "an open-source web application that allows you to
create and share documents that contain live code,
equations, visualizations and narrative text."
It is frequently used to experiment with snippets of Python code.

JupyterLab is "Jupyter’s Next-Generation Notebook Interface".

## Getting Started

To install JupyterLab, enter `pip install jupyterlab`.

To run JupyterLab, enter `jupyter lab`.
This starts a local notebook server that listens on port 8888
and opens a browser tab to return the user interface
for a browser-based IDE.
{% aTargetBlank "https://github.com/lambdalisue/jupyter-vim-binding",
"jupyter-vim-binding" %} is an extension that supports Vim key bindings.

The left sidebar can contain many things depending on
which of the icons on the left is activated.

| Sidebar Content          | Icon Description                         |
| ------------------------ | ---------------------------------------- |
| file browser             | dark gray folder icon                    |
| list of opened terminals | circle containing a square               |
| commands                 | document with a magnifying glass over it |
| property inspector       | two gears                                |
| list of opened tabs      | white folder icon                        |
| extensions manager       | puzzle piece                             |

To open a terminal tab,
display the file browser in the left sidebar,
click "+" in the upper left,
and click the "Terminal" button.
This uses the default shell.
The tab title shows the current working directory,
but it is truncated and hovering over it does not show the complete path.
A text-based editor such as Vim can be used in a terminal tab to edit files.

To change the terminal theme,
select Settings ... Terminal Theme ... {theme}
where the options are Inherit, Light, and Dark.

To open a file in a new tab,
display the file browser in the left sidebar and double-click a file.
New files and directories appear in the file browser after about 5 seconds.
To see them sooner, press the reload icon at the top of the file browser.
Right-click in the file browser to
copy, create, delete, download, open, or rename a file or folder.

Many image formats are supported and can be opened in a tab.
Scroll to position the visible portion of the image.
Press the "=" and "-" keys to zoom in and out.
Press the "[" and "]" keys to rotate left and right.
Press the "0" key to reset the zoom and rotation.

Other document formats that are supported include
CSV, HTML, JSON, Jupyter Notebooks, PDF, and Latex.
For CSV files, the delimiter can be changed to
comma, semicolon, tab, pipe, or hash.

## Text Editor

To open a text file in a new tab,
open the file browser in the left sidebar and double-click the file.

To edit using preferred key bindings,
select Settings ... Text Editor Key Map ... {key map name}
where the options are default, Sublime Text, vim, and emacs.

To change the editor color theme,
select Settings ...Text Editor Theme ... {theme name}.
At last check there were 15 themes that include
"solarized dark" and "solarized light".

## Jupyter Notebooks

To create a notebook, click the commands icon in the left sidebar
and type or select "New Notebook".
A dialog opens to select a kernel.
By default the only available options are "Python 3" and "No Kernel".
Select one and press the "Select" button.

The new notebook will have the name "Untitled" followed by a number
and the file extension ".ipynb" for "Interactive Python Notebook".
Notebooks are stored in JSON format.
Their "cells" property is an array of objects that describe each cell.

To rename a notebook, right-click the tab and select "Rename Notebook".

To open an existing notebook, double-click it in the file browser.

Notebooks open in command mode
which supports navigating and changing layout.
Press the arrow keys to navigate between cells.
The current cell is highlighted with ?.

Keyboard shortcuts, many of which are borrowed from Vim, include:

- return - edit current cell
- shift-return - stop editing current cell and execute it
- esc - stop editing current cell and do not execute it
- a - add a cell above current one
- b - add a cell below current one
- c - copy the current cell
- dd - delete the current cell
- j or down arrow - move down one cell
- shift-j or shift down arrow - select current cell and next one down
- k or up arrow - move up one cell
- shift-k or shift up arrow - select current cell and previous one up
- m - change format of current cell to Markdown
- v - pasted copied cell into current cell
- x - cut current cell
- y - change format of current cell to code
- z - undo last action
- shift-z - redo last undo
- ctrl-shift-minus - split current cell into two

To select multiple cells,

To reorder cells drag them up or down by the square brackets to their left
to a new location.
Another option is to cut the current cell,
move to the cell it should be placed after,
and paste it.

Yet another option is to customize key bindings.
For example, ctrl-up arrow and ctrl-down arrow can be mapped to the
"notebook:move-cell-up" and "notebook:move-cell-down" commands.
To this, select Settings ... Advanced Settings Editor,
select "Keyboard Shortcuts",
and add the following User Preferences:

```json
{
  "shortcuts": [
    {
      "command": "notebook:move-cell-down",
      "keys": ["Ctrl ArrowDown"],
      "selector": "body"
    },
    {
      "command": "notebook:move-cell-up",
      "keys": ["Ctrl ArrowUp"],
      "selector": "body"
    }
  ]
}
```

To save changes, click the floppy disk icon or press ctrl-s (cmd-s on Mac).

Markdown cells can contain equations surrounded by $ characters.
For example, "The Pythagorean theorem is $ x^2 + y^2 = r^2 \$."

URLs in Markdown cells are automatically converted to hyperlinks.

Code cells are automatically executed after leaving edit mode.
Their output is displayed below the cell.
If no output appears, look for error messages in the terminal
where the Jupyter server is running.

The number in square brackets to the left of a cell
indicates to the order in which the cell was executed
relative to the other code cells.

To execute a cell again without going into edit mode,
select the cell and click the triangle at the top of the notebook.
To re-execute all code cells, click the "commands" icon in the left sidebar
and enter or select "Run All Cells".

Variables defined in preceding code cells are available in cells that follow.
Ending a cell with a variable name causes its value to be output
without using a `print` statement.

TODO: How do you move the current cell up or down to re-order them?

A code cell can import and use modules. For example:

```python
from statistics import Statistics
stats = Statistics(1, 2, 3, 4, 5, 6)
stats.report()
```

See tips at https://www.dataquest.io/blog/jupyter-notebook-tips-tricks-shortcuts.
