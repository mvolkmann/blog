---
eleventyNavigation:
  key: tkinter
  order: 7
  parent: Python
layout: topic-layout.njk
---

## Overview

The name "tkinter" is short for "Tk interface".
It is a Python interface to
{% aTargetBlank "https://tkdocs.com/", "Tk" %} which is a GUI toolkit
originally created for the
{% aTargetBlank "https://www.tcl.tk/", "Tcl" %} programming language.
The Tk license allows free commercial usage.

The official documentation for tkinter can be found
{% aTargetBlank "https://docs.python.org/3/library/tkinter.html", "here" %}.
Better documentation can be found at {% aTargetBlank
"https://www.tutorialspoint.com/python3/python_gui_programming.htm",
"tutorialspoint" %}.
Many of the links on this page are to pages of the tutorialspoint site.

## Alternatives

Alternative GUI libraries for Python include:

- {% aTargetBlank "https://www.jython.org/", "Jython" %}  
  This is a Java implementation of Python that enables use of
  Java-based GUI libraries such as JavaFX and Swing
  whose licenses allow free commercial usage.

- {% aTargetBlank "https://kivy.org/", "Kivy" %}  
  This library was officially released in 2011.
  Apps that use it run on Linux, Windows, macOS, Android, and iOS.
  The Kivy license allows free commercial usage.

- {% aTargetBlank "https://riverbankcomputing.com/software/pyqt/intro", "PyQt" %}  
  This is a set of Python bindings for the {% aTargetBlank
  "https://riverbankcomputing.com/software/pyqt", "Qt" %} C++ GUI framework.
  The Qt license does not allow free commercial usage.
  At last check the cost of commercial licenses was \$550 USD per developer.

- {% aTargetBlank "https://www.wxpython.org/", "wxPython" %}  
  This is a set of Python bindings for the {% aTargetBlank
  "https://wxwidgets.org/", "wxWidgets" %} C++ GUI library.
  Apps that use it run on Linux, Windows, and macOS.
  It uses platform-specific widgets.
  The wxWidgets license allows free commercial usage.

Characteristics of tkinter that distinguish it from these alternatives
are that it is easier to use and it comes pre-installed with Python.

## Simple Example

Let's create a GUI app that just renders a label.
Running the following code opens a window that displays text.

```python
from tkinter import *

root = Tk() # a kind of window that inherits from Wm (window manager)
root.geometry('800x600') # sets initial window size to width x height
label = Label(text='Hello, World!') # one of the many supported widgets
label.pack() # simplest of three layout methods
root.mainloop()
```

The `mainloop` method starts an event loop which
blocks code that follows from executing.
The event loop processes user interface events.
Closing the window causes the event loop to terminate
and allows code that follows to execute.
Assuming no additional blocking calls are made,
the end of the program is reached and it exits.

## Widgets

Widgets can be categorized into two groups,
those that act as containers for other widgets
and those that do not.

### Container Widgets

| Name                                                                                        | Description                                                 | Nearest DOM/HTML Equivalent |
| ------------------------------------------------------------------------------------------- | ----------------------------------------------------------- | --------------------------- |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_frame.htm", "`Frame`" %}         | container                                                   | `<div>`                     |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_labelframe", "`LabelFrame`" %}   | labeled container                                           | `<fieldset>`                |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_panedwindow", "`PanedWindow`" %} | horizontal or vertical set of children<br>that is resizable | `<div>`                     |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_toplevel", "`Tk`" %}             | root window                                                 | DOM `Window`                |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_toplevel", "`Toplevel`" %}       | additional window                                           | DOM `Window`                |

### Non-container Widgets

| Name                                                                                        | Description                  | Nearest DOM/HTML Equivalent                 |
| ------------------------------------------------------------------------------------------- | ---------------------------- | ------------------------------------------- |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_button", "`Button`" %}           | button that can be pressed   | `<button>`                                  |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_canvas", "`Canvas`" %}           | for drawing lines and shapes | `<canvas>` or `<svg>`                       |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_checkbutton", "`Checkbutton`" %} | checkbox                     | `<input type="checkbox">`                   |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_entry", "`Entry`" %}             | single-line text input       | `<input type="text">`                       |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_label", "`Label`" %}             | label                        | `<label>`                                   |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_listbox", "`Listbox`" %}         | drop-down selection          | `<select>` with `<option>` children         |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_menu", "`Menu`" %}               | menu bar or menu             | none                                        |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_menubutton", "`Menubutton`" %}   | menu item                    | none                                        |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_radiobutton", "`Radiobutton`" %} | radio button                 | `<input type="radio">`                      |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_scale", "`Scale`" %}             | slider                       | `<input type="range">`                      |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_scrollbar", "`Scrollbar`" %}     | scrollbar                    | `<div>` with CSS `overflow` set to `scroll` |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_text", "`Text`" %}               | multi-line text input        | `<textarea>`                                |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_spinbox", "`Spinbox`" %}         | number input                 | `<input type="number">`                     |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_messagebox", "`tkMessageBox`" %} | dialog box                   | `alert`, `confirm`, `prompt`                |

## Layout Methods

| Layout Method                                                                       | Details                                             |
| ----------------------------------------------------------------------------------- | --------------------------------------------------- |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_pack.htm", "`pack`" %}   | arranges widgets on the four sides of container     |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_place.htm", "`place`" %} | places widgets and specific positions in container  |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_grid.htm", "`grid`"  %}  | places widgets at row/column positions in container |

In the tables below:

- Values that are all uppercase are constants defined by tkinter, not strings.
  Typically their values are the lowercase string version of the name.
- Percentages are float values between zero and one.

### `pack`

The {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_pack.htm",
"`pack`" %} method accepts the following options:

| Option   | Values                                                                                           |
| -------- | ------------------------------------------------------------------------------------------------ |
| `expand` | fill unused space horizontally and vertically;<br>`False` (default) or `True`                    |
| `fill`   | similar to `expand`, but can retain minimal dimensions;<br>`NONE` (default), `X`, `Y`, or `BOTH` |
| `side`   | `TOP` (default), `BOTTOM`, `LEFT`, or `RIGHT`                                                    |

Here is a simple example of using `pack`:

```python
from tkinter import *

root = Tk()
root.geometry('300x200')

bg = 'yellow'
relief = RAISED

Label(text='One', bg=bg, relief=RAISED).pack(side=TOP)
Label(text='Two', bg=bg, relief=RAISED).pack(side=RIGHT)
Label(text='Three', bg=bg, relief=RAISED).pack(side=BOTTOM)
Label(text='Four', bg=bg, relief=RAISED).pack(side=LEFT)

Label(text='Five', bg=bg, relief=RAISED).pack(side=TOP)
Label(text='Six', bg=bg, relief=RAISED).pack(side=RIGHT)
Label(text='Seven', bg=bg, relief=RAISED).pack(side=BOTTOM)
Label(text='Eight', bg=bg, relief=RAISED).pack(side=LEFT)

root.mainloop()
```

![pack example](/blog/assets/tkinter-pack.png)

### `place`

The {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_place.htm",
"`place`" %} method accepts the following options:

| Option       | Values                                                                                |
| ------------ | ------------------------------------------------------------------------------------- |
| `anchor`     | compass direction<br>`N`, `S`, `E`, `W`, `NE`, `NW` (default), `SE`, or `SW`          |
| `bordermode` | `INSIDE` or `OUTSIDE`;indicates whether<br>border is included in `height` and `width` |
| `height`     | in pixels                                                                             |
| `width`      | in pixels                                                                             |
| `relheight`  | percentage of parent height                                                           |
| `relwidth`   | percentage of parent width                                                            |
| `relx`       | location as percentage of parent width                                                |
| `rely`       | location as percentage of parent height                                               |
| `x`          | location in pixels                                                                    |
| `y`          | location in pixels                                                                    |

### `grid`

The {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_grid.htm",
"`grid`"  %} method accepts the following options:

| Option       | Values                                                                                                              |
| ------------ | ------------------------------------------------------------------------------------------------------------------- |
| `column`     | column index; default is 0                                                                                          |
| `columnspan` | # of columns to occupy; default is 1                                                                                |
| `ipadx`      | internal x padding; default is 0                                                                                    |
| `ipady`      | internal y padding; default is 0                                                                                    |
| `padx`       | external x padding (margin); default is 0                                                                           |
| `pady`       | external y padding (margins); default is 0                                                                          |
| `row`        | row index; default is 0                                                                                             |
| `rowspan`    | # of rows to occupy; default is 1                                                                                   |
| `sticky`     | position if cell is larger; centered by default;<br>compass direction `N`, `S`, `E`, `W`, `NE`, `NW`, `SE`, or `SW` |
