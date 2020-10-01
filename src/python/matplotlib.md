---
eleventyNavigation:
  key: mapplotlib
  order: 6
  parent: Python
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://matplotlib.org/", "matplotlib" %}
is a Python library for data visualization.

To install this package, enter `pip install matplotlib`.

The most commonly used submodule in matplotlib is `pylot`.
To import this, use one of the following:

```python
from matplotlib import pyplot as plt
import matplotlib.pyplot as plt
```

Note that by convention the `plt` alias is used to refer to `pyplot`.

## Line Charts

To create a basic line chart:

```python
from matplotlib import pyplot as plt
import numpy as np

student_ids = np.arange(1, 5) # [1, 2, 3, 4]
test1_scores = [75, 90, 65, 95]
test2_scores = [80, 85, 80, 100]
plt.plot(student_ids, test1_scores)
plt.plot(student_ids, test2_scores)
plt.show()
```

This produces the chart below:

![line chart #1](/blog/assets/line-chart-1.png)

The chart automatically scales to accommodate the data
and the x and y tick marks are automatically determined.

There are many ways to enhance this chart as shown in the table below.

| Enhancement         | Code                                            |
| ------------------- | ----------------------------------------------- |
| add chart title     | `plt.title('Test Scores')`                      |
| add x-axis label    | `plt.xlabel('Student')`                         |
| add y-axis label    | `plt.ylabel('Score')`                           |
| add label to a line | `plt.plot(x_values, y_values, label='test #1')` |
| set color of a line | `plt.plot(x_values, y_values, color='red')`     |
| set width of a line | `plt.plot(x_values, y_values, linewidth=3)`     |
| set line style      | `plt.plot(x_values, y_values, linestyle='--')`  |
| add legend          | `plt.legend()`                                  |
| set x ticks         | `plt.xticks(student_ids)`                       |
| set y ticks         | `plt.yticks(y_ticks)`                           |

The `xticks` and `yticks` methods override tick marks
that are automatically provided.
They are desirable in this case because our student numbers are integers,
not values like 1.5 or 2.5.

To change the font uses, add a `fontdict` argument
to some of these method calls.
For example, to change the font used for the chart title
we can change the call to the `title` method as follows:

```python
plt.title('Test Scores', fontdict={'fontname': 'Comic Sans MS','fontsize': 24})
```

Colors can be specified by name or by hex code following by a pound sign.
To specify the color of a line, add a `color` argument.

The lines are solid by default.
To specify another line style, add a `linestyle` argument.
Supported line styles include:

| Style   | Syntax                |
| ------- | --------------------- |
| solid   | `'solid'` or `'-'`    |
| dotted  | `'dotted'` or `':'`   |
| dashed  | `'dashed'` or `'--'`  |
| dashdot | `'dashdot'` or `'-.'` |

For more line style variations, see {% aTargetBlank
"https://matplotlib.org/3.1.0/gallery/lines_bars_and_markers/linestyles.html",
"matplotlib Linestyles" %}.

Markers are shapes that can be rendered at data points.
The data points on the lines do not include markers by default.
To add markers to a line, specify a `marker` argument.
Marker values include:

| Marker          | Syntax |
| --------------- | ------ |
| point           | `'.'`  |
| pixel           | `','`  |
| circle          | `'o'`  |
| plus            | `'+'`  |
| x               | `'x'`  |
| diamond         | `'D'`  |
| diamond thin    | `'d'`  |
| triangle down   | `'v'`  |
| triangle up     | `'^'`  |
| triangle left   | `'<'`  |
| triangle right  | `'>'`  |
| square          | `'s'`  |
| pentagon        | `'p'`  |
| hexagon         | `'h'`  |
| hexagon rotated | `'H'`  |
| octagon         | `'8'`  |
| plus filled     | `'P'`  |
| star            | `'*'`  |

For more marker variations, see {% aTargetBlank
"https://matplotlib.org/api/markers_api.html",
"matplotlib Markers" %}.

To change the size of markers, specify a `markersize` argument.

Markers are composed of two parts, an edge (border) and a face (interior).
By default, both have the same color as the line on which they appear.
To change the marker color, specify the
`markeredgecolor` and/or `markerfacecolor` arguments.

Let's try some of these enhancements.

```python
from matplotlib import pyplot as plt
import numpy as np

student_ids = np.arange(1, 5) # [1, 2, 3, 4]
test1_scores = [75, 90, 65, 95]
test2_scores = [80, 85, 80, 100]
plt.plot(
    student_ids, test1_scores, color='red',
    label='test #1', linestyle='--', linewidth=3, marker='o',
    markersize=7, markerfacecolor='white')
plt.plot(
    student_ids, test2_scores, color='blue',
    label='test #2', linestyle=':',
    marker='s', markerfacecolor='white')
plt.title(
    'Test Scores',
    fontdict={'fontname': 'Comic Sans MS','fontsize': 24})
plt.xlabel('Student')
plt.ylabel('Score')
plt.legend()
plt.xticks(student_ids)
plt.show()
```

With all of these enhancements in place, the chart becomes the following:

![line chart #2](/blog/assets/line-chart-2.png)

To save a plot in a PNG file:

```python
plt.savefig(file_path)
```

It's easy to plot functions.
For example:

```python
from matplotlib import pyplot as plt
import numpy as np

f = lambda x: x**2 / 10
x_values = np.arange(0, math.pi*2, 0.1)
plt.plot(x_values, np.sin(x_values), label='sin')
plt.plot(x_values, np.cos(x_values), label='cos')
plt.plot(x_values, f(x_values), label='x^2 / 10')
plt.title('Some Functions')
plt.xlabel('radians')
plt.legend()
plt.plot()
```

![line chart #3](/blog/assets/line-chart-3.png)

TODO: Add something on `plt.gcf().tight_layout()`
which adjusts the padding between and around subplots.
What is a subplot?

TODO: How do you change the chart background color?

TODO: Cover xkcd theme.

More detail is coming soon!
