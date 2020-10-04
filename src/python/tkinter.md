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

| Name                                                                                        | Description                                                                                      | Nearest DOM/HTML Equivalent                            |
| ------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ | ------------------------------------------------------ |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_button", "`Button`" %}           | button that can be pressed;<br>`command` option specifies function to call when clicked.         | `<button>`                                             |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_canvas", "`Canvas`" %}           | for drawing lines and shapes                                                                     | `<canvas>` or `<svg>`                                  |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_checkbutton", "`Checkbutton`" %} | checkbox                                                                                         | `<input type="checkbox">`                              |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_entry", "`Entry`" %}             | single-line text input<br>`command` option specifies function to call<br>when user changes value | `<input type="text">`                                  |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_label", "`Label`" %}             | label                                                                                            | `<label>`                                              |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_listbox", "`Listbox`" %}         | drop-down selection                                                                              | `<select>` with `<option>` children                    |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_menu", "`Menu`" %}               | menu bar or menu;<br>call `add_*` methods to add menu items<br>with a `label` and a `command`    | none                                                   |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_menubutton", "`Menubutton`" %}   | menu name that can be clicked to open a menu                                                     | none                                                   |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_radiobutton", "`Radiobutton`" %} | radio button                                                                                     | `<input type="radio">`<br>See example at link to left. |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_scale", "`Scale`" %}             | slider                                                                                           | `<input type="range">`                                 |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_scrollbar", "`Scrollbar`" %}     | scrollbar                                                                                        | `<div>` with CSS `overflow` set to `scroll`            |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_text", "`Text`" %}               | multi-line text input                                                                            | `<textarea>`                                           |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_spinbox", "`Spinbox`" %}         | number input                                                                                     | `<input type="number">`                                |
| {% aTargetBlank "https://www.tutorialspoint.com/python3/tk_messagebox", "`tkMessageBox`" %} | dialog box;<br>see `ask*` and `show*` methods                                                    | `alert`, `confirm`, `prompt`, and `<dialog>`           |

The following components accept a `command` option that specifies a function
to be called when the user clicks them or changes their value:
`Button`, `Checkbutton`, `Entry`, `Radiobutton`,
`Scale`, `Scrollbar`, and `Spinbox`.

The `Listbox` widget does not support the `command` option.
To trigger a call to a function when the selected lines change,
listen for a virtual event as follows:

```python
def print_selected_indexes(event):
    selected_indexes = myListbox.curselection() # a tuple
    print('selected_indexes =', selected_indexes)

myListbox.bind('<<ListboxSelect>>', print_selected_indexes)
```

The `Text` widget also does not support the `command` option.
To trigger a call to a function when the selected lines change,
listen for a virtual even as follows:

```python
def print_text(event):
    # '-1c' removes newline from end
    value = text.get('1.0', 'end-1c') + event.char
    print('value =', value)

myText.bind('<Key>', print_text)
```

The following components accept a `state` option that defaults to `NORMAL`,
but has the value `ACTIVE` when the mouse is over it
and can be set to `DISABLED` to disable it:
`Button`, `Checkbutton`, `Entry`, `Menubutton`, `Radiobutton`,
`Scale`, `Text`, and `Spinbox`.

The `Listbox` widget does not support the `state` option.
However, the `configure` method can be called on any widget to set its options.
This can be used to set the `state` of a `Listbox` as follows:

```python
myListbox.configure(state=DISABLED)
```

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

def add_button(text, side):
    btn = Button(root, padx=5, pady=5, text=text)
    btn.pack(side=side)

add_button('One', TOP)
add_button('Two', RIGHT)
add_button('Three', BOTTOM)
add_button('Four', LEFT)

add_button('Five', TOP)
add_button('Six', RIGHT)
add_button('Seven', BOTTOM)
add_button('Eight', LEFT)

root.mainloop()
```

TODO: Why is centering off here?

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
