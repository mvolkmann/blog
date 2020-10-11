---
eleventyNavigation:
  # key: Plotly
  order: 7
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

## Overview

{% aTargetBlank "https://plotly.com/python/", "Plotly" %}
is a Python library for creating interactive data visualizations.
It builds on the {% aTargetBlank "https://plotly.com/javascript/",
"Plotly JavaScript library" %} which uses [D3](../../d3/01-overview/).

To install this package, enter `pip install plotly`.

To use Plotly inside a JupyterLab, assuming that is already installed, enter:

```bash
pip install "ipywidgets>=7.5"
jupyter labextension install jupyterlab-plotly@4.11.0
jupyter labextension install @jupyter-widgets/jupyterlab-manager plotlywidget@4.11.0
```

## Command-line Example

This example generates a simple bar chart.

```python
import plotly.graph_objects as go

data = [2, 3, 1]

# Create a bar chart using these values.
fig = go.Figure(data=go.Bar(y=data))

# Generate HTML for this bar chart, write it to a file,
# and open the file in the default web browser.
# The HTML consists of:
# - a script tag that configures Plotly
# - a script tag contains the Plotly library implementation
# - a script that that uses Plotly to draw the chart
# - a mostly empty svg element where the plot is created
fig.write_html('bar-chart.html', auto_open=True)
```

## JupyterLab Example

This generates the same bar chart, but inside a Jupyter Notebook
using JupyterLab.
Enter `jupyter lab` to start it.
Create a new Python notebook and enter the following:

```python
import plotly.graph_objects as go

data = [2, 3, 1]
fig = go.Figure(data=go.Bar(y=data))
fig.show()
```

Note: This doesn't render a plot when run in
JupyterLab launched from Anaconda Navigator!

## Provided Datasets

The following code can be entered in a Jupyter Notebook
to render a line chart that shows stock prices.
It uses a dataset provided by Plotly in {% aTargetBlank
"https://plotly.com/python-api-reference/generated/plotly.data.html#module-plotly.data",
"`plotly.express.data`" %}.
These include:

| Dataset Name     | Description                                                                         |
| ---------------- | ----------------------------------------------------------------------------------- |
| carshare         | availability of car-sharing services in Montreal over a month-long period           |
| election         | voting results for a district in the 2013 Montreal mayoral election                 |
| election_geojson | each feature represents an electoral district in the 2013 Montreal mayoral election |
| experiment       | results of 100 simulated participants on three hypothetical experiments             |
| gapminder        | life expectancy, population, and GDP for countries in a given year                  |
| iris             | flower data                                                                         |
| medals_long      | Olympic medals for short track speed skating                                        |
| medals_wide      | variation on previous data                                                          |
| stocks           | closing prices from six tech stocks in 2018 and 2019                                |
| tips             | tips on restaurant bills                                                            |
| wind             | wind intensity in a cardinal direction and its frequency                            |

## Line Chart

```python
import plotly.express as px
import plotly.graph_objects as go

df = px.data.stocks()
px.line(
    df,
    x='date', y=['AAPL', 'GOOG'],
    labels={'x': 'Date', 'y': 'Price'},
    title="Apple vs. Google")
```

Hover over a data point to see a popup that shows its name and x/y values.

```python
fig = go.Figure()
fig.add_trace(go.Scatter(x=df.date, y=df.AAPL, mode='lines', name='Apple'))
fig.add_trace(go.Scatter(x=df.date, y=df.AMZN, mode='lines+markers', name='Amazon'))
fig.add_trace(go.Scatter(
    x=df.date, y=df.GOOG, mode='lines+markers', name='Google',
    line=dict(color='firebrick', width=2, dash='dashdot')))
```
