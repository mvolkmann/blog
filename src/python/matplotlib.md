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

To create a line chart:

```python
# Convention is to use plt when referring to pyplot.
from matplotlib import pyplot as plt

x_values = []
y_values = []
plt.plot(x_values, y_values)
plt.show()
```

More detail is coming soon!
