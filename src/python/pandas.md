---
eleventyNavigation:
  key: Pandas
  order: 4
  parent: Python
layout: topic-layout.njk
---

## Overview

{% aTargetBlank "https://pandas.pydata.org/", "pandas" %}
is a Python library for data analysis.
For details, see the
{% aTargetBlank "https://pandas.pydata.org/docs/reference/", "API Reference" %}.

To install pandas, enter `pip install pandas`.

Pandas provides two primary data structures.
The `Series` class is used to represent a series of data values
and can be thought of as a single-column spreadsheet.
The `DataFrame` class is used to represent a table of data values
and can be thought of as a multi-column spreadsheet.
It is used more frequently than `Series`.

## Loading Data

A `DataFrame` can be created from many sources including
CSV files, Excel spreadsheets, dict objects, and NumPy arrays.
To create a `DataFrame` from a CSV file,

```python
covid_df = pd.read_csv('us-states-covid-data.csv')

dogs_df = pd.DataFrame([
    {'name': 'Maisey', 'breed': 'Treeing Walker Coonhound'},
    {'name': 'Oscar', 'breed': 'German Shorthaired Pointer'},
    {'name': 'Ramsay', 'breed': 'Native American Indian Dog'},
    {'name': 'Comet', 'breed': 'Whippet'}
])
dogs_df.at[1, 'name'] = 'Oscar Wilde' # changes name property in row with index 1
```

## Viewing Data

| Operation                | Code                               |
| ------------------------ | ---------------------------------- |
| get column data types    | `df.dtypes`                        |
| display first n rows     | `df.head(n)` where n defaults to 5 |
| display last n rows      | `df.tail(n)` where n defaults to 5 |
| display row index values | `df.index`                         |
| display column names     | `df.columns`                       |

## Modifying Data

| Operation         | Code                       |
| ----------------- | -------------------------- |
| change cell value | `df.at[row_index, column]` |

## Creating New DataFrames

| Operation                | Code                                  |
| ------------------------ | ------------------------------------- |
| sort on one column       | `df.sort_values(by='col-name')`       |
| sort on multiple columns | `df.sort_values(by=['col1', 'col2'])` |
| transpose                | `df.T`                                |

To sort in place, add `inplace=True`.

## Creating Other Representations

| Operation          | Code            |
| ------------------ | --------------- |
| create NumPy array | `df.to_numpy()` |

## Statistics

| Operation                            | Code            |
| ------------------------------------ | --------------- |
| get basic statistics for all columns | `df.describe()` |

More detail is coming soon!
