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
If you close the browser tab and wish to open a new one
to connect to the running server, browse localhost:8888.

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

Tabs can be dragged to a new location similar to VS Code.
This includes dragging them so that multiple tabs are visible at the same time.

Many image formats are supported and can be opened in a tab.
Scroll to position the visible portion of the image.
Press the "=" and "-" keys to zoom in and out.
Press the "[" and "]" keys to rotate left and right.
Press the "0" key to reset the zoom and rotation.

Other document formats that are supported include
CSV, HTML, JSON, Jupyter Notebooks, PDF, and Latex.

CSV parsing is highly performant and a table view is lazily rendered,
enabling processing of very large CSV files.
The delimiter can be changed to comma, semicolon, tab, pipe, or hash.

## Text Editor

To open a text file in a new tab,
open the file browser in the left sidebar and double-click the file.

To edit using preferred key bindings,
select Settings ... Text Editor Key Map ... {key map name}
where the options are default, Sublime Text, vim, and emacs.
There are also extensions that enable use of Vim key bindings in
notebook cells, but I haven't been able to install one without errors.

To change the editor color theme,
select Settings ...Text Editor Theme ... {theme name}.
At last check there were 15 themes that include
"solarized dark" and "solarized light".

When editing Markdown files, to see a preview
right-click and select "Show Markdown Preview".
This opens a new tab for the preview to the right of the Markdown tab.

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
- shift-return - stop editing current cell, execute it,
  and advance to the next cell
- ctrl-return - stop editing current cell, execute it,
  and remain on the current cell
- esc - stop editing current cell and do not execute it
- a - add a cell above current one
- b - add a cell below current one
- c or press copy icon - copy the current cell
- dd or press scissors icon - delete the current cell
- j or down arrow - move down one cell
- shift-j or shift down arrow - select current cell and next one down
- k or up arrow - move up one cell
- shift-k or shift up arrow - select current cell and previous one up
- m - change format of current cell to Markdown
- v or press paste icon (clipboard) -
  paste copied cell into current cell
- x - cut current cell
- y - change format of current cell to code
- z - undo last action
- shift-z - redo last undo
- ctrl-shift-minus - split current cell into two

Another way to select multiple cells starting from the current one
is to shift-click the cell at the other end of the range
(above or below the current one).

To collapse a cell or the output of a code cell,
click the vertical blue bar to its left.
There is one blue bar for the cell and one for its output.
To expand, click the blue vertical line again.
The View menu contains many menu items that
collapse and expand sets of cells and outputs.

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

To toggle the display of line numbers inside cells,
select View ... Show Line Numbers.

The number in square brackets to the left of a cell
indicates to the order in which the cell was executed
relative to the other code cells.
The number will be preceded by "\*" while the code is running.
This will only be visible for long running code.
While it is running, the kernel is locked and no other cells can run.
To stop execution of a long-running cell, generate a KeyboardInterrupt
by clicking the black square at the top or pressing the "i" key twice.
To demonstrate this, enter the following code in a cell and execute it:

```python
from time import sleep
for i in range(10):
    print(i)
    sleep(1) # sleeps for one second
```

To execute a cell again without going into edit mode,
select the cell and click the triangle at the top of the notebook.
To re-execute all code cells, click the "commands" icon in the left sidebar
and enter or select "Run All Cells".

Variables defined in preceding code cells are available in cells that follow.
Ending a cell with a variable name causes its value to be output
without using a `print` statement.

A code cell can import and use modules. For example:

```python
from statistics import Statistics
stats = Statistics(1, 2, 3, 4, 5, 6)
stats.report()
```

To view a notebook in multiple tabs,
each of which can be scrolled to a different position,
select File ... New View for Notebook.
Cells can be dragged from one view to another
and even between notebooks.
TODO: This copies the cell. Is there a way to move it instead?

To export a notebook to a `.py` file,
select File ... Export Notebook As ... Export Notebook to Executable Script.
Select a directory, enter a file name, and press "Save".
Markdown cells will appear as Python comments.
Code cells will be preceded by a comment that reads "# In[number]".

To export a notebook as a PDF file,
select File ... Export Notebook As ... Export Notebook to PDF.
This requires nbconvert and Pandoc be installed.
To install nbconvert, enter `pip install nbconvert`.
To install Pandoc in macOS, enter `brew install pandoc xelatex`.
Also see {% aTargetBlank
"https://nbconvert.readthedocs.io/en/latest/install.html#installing-tex",
"Install TeX" %} for platform-specific install instructions.
This is a very large install! I did not try this.

See tips at {% aTargetBlank
"https://www.dataquest.io/blog/jupyter-notebook-tips-tricks-shortcuts",
"Jupyter Notebook Tips, Tricks, & Shortcuts" %}.

## JavaScript Kernel

Jupyter supports Python out of the box.
Additional language kernels can be installed
to support other programming languages.
These implement the Jupyter messaging protocol.

{% aTargetBlank "https://github.com/n-riesco/ijavascript", "IJavascript" %}
is a Jupyter kernel for JavaScript.
To install it:

- install {% aTargetBlank "https://nodejs.org/", "Node.js" %}
- see the instructions at the IJavascript web site above
  (note that this library consistently uses
  the wrong case for the "S" in JavaScript)
- on macOS:
  - brew install pkg-config zeromq
  - sudo easy_install pip
  - pip install --upgrade pyzmq jupyter
  - npm install -g ijavascript
  - ijsinstall
- for other operating systems, see
  {% aTargetBlank "https://github.com/n-riesco/ijavascript#installation",
  "Installation" %}

To create a "Javascript (Node.js)" notebook,
click the File Explorer icon in the left sidebar,
click the "+" at the top of the left sidebar,
and click the "JS" box under "Notebook".
Cells that have a type of "Code" can contain and execute JavaScript code.

## Plots

There are many libraries that can be used to create plots inside Jupyter.
A popular option is {% aTargetBlank "https://matplotlib.org/", "matplotlib" %}.
To install it, enter `pip install matplotlib`.

To draw a line chart, enter the following in a cell and execute it:

```python
%matplotlib inline
from matplotlib import pyplot
points = [[0,0], [1,4], [2,2], [3,1], [4,6]]
pyplot.plot(points, linewidth=1)
pyplot.show()
```

To plot a function, enter the following in a cell and execute it:

```python
from matplotlib import pyplot as plt
import numpy as np
import math # for pi
step_size = 0.05
min_x = 0
#min_x = -5 #0
max_x = math.pi * 2
#max_x = 5
x = np.arange(min_x, max_x, step_size)
y = np.sin(x)
#y = x**2
plt.plot(x, y, color='red')
plt.axhline(0, color='black', linestyle="dotted", linewidth=1) # x axis
plt.axvline(0, color='black', linestyle="dotted", linewidth=1) # y axis
plt.xlabel('angle in radians')
plt.ylabel('sine')
plt.title('sine wave')
plt.show()
```

To draw a bar chart, enter the following in a cell and execute it:

```python
%matplotlib inline
from matplotlib import pyplot as plt
figure = plt.figure()
axes = figure.add_axes([0,0,1,1])
names = ['Mark', 'Tami', 'Amanda', 'Jeremy']
scores = [100, 200, 150, 300]
axes.bar(
    names,
    scores,
    color=['red', 'orange', 'yellow', 'green'],
    edgecolor='black')
axes.set_title('Game Scores')
axes.set_xlabel('Name')
axes.set_ylabel('Score')
plt.show()
```

To save a plot in a `.png` file:

```python
file_path = 'game-scores.png'
figure.savefig(file_path, bbox_inches='tight', dpi=600)
```

## Vega

{% aTargetBlank "https://vega.github.io/vega/", "Vega" %}
is a JSON format for describing data visualizations.
Jupyter has builtin support for rendering Vega files
with a `.vg` or `.vg.json` file extension.
There are many examples at the Vega website linked above.
Copy the JSON for one into a `.vg` file
and open it in Jupyter to see the result.

Click the circled ellipsis in the upper-right
to display a menu where you can select
"Save as SVG", "Save as PNG",
"View Source" (displays read-only text in a new browser tab),
or "Open in Vega Editor" (opens Vega online editor in a new browser tab).
Changes made in the Vega editor can be exported as JSON and reopened in Jupyter.

## GeoJSON

To render GeoJSON data in Jupyter:

- `jupyter labextension install @jupyterlab/geojson-extension`
- may need to restart the Jupyter server
- enter code like the following in a cell of a Python notebook

```python
from IPython.display import GeoJSON

GeoJSON({
    'type': 'Feature',
    'geometry': {
        'type': 'Point',
        #'coordinates': [-118.4563712, 34.0163116]
        'coordinates': [-90.594482, 38.709419]
        TODO: How do you set the zoom level?
    }
})
```

## Resources

For help, join the Jupyter Discourse by browsing the
{% aTargetBlank "https://jupyter.org/community", "Community" %} page
and clicking the "View" button for "Jupyter Discourse".

For data analysis, see
{% aTargetBlank "https://pandas.pydata.org/", "pandas" %}.
TODO: Learn more about Pandas!
