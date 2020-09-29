---
eleventyNavigation:
  key: Pandas
  order: 4
  parent: Python
layout: topic-layout.njk
---

## Outstanding Questions

Is a DataFrame a collection of Series objects?
Do DataFrame methods always return a new DataFrame?
Do the new DataFrame objects share Series and cells
with ones they were created from?

Pandas DataFrames use Numpy arrays.

## Overview

{% aTargetBlank "https://pandas.pydata.org/", "pandas" %}
is a Python library for data analysis.
For details, see the
{% aTargetBlank "https://pandas.pydata.org/docs/reference/", "API Reference" %}.

To install pandas, enter `pip install pandas`.

To import the pandas library, use `import pandas as pd`.
By convention, "pd" is the name used to refer to the library in code.

Pandas provides two primary data structures.
The `Series` class is used to represent a series of data values
and can be thought of as a single-column spreadsheet.
The `DataFrame` class is used to represent a table of data values
and can be thought of as a multi-column spreadsheet.
It is used more frequently than `Series`.

## Loading Data

A `DataFrame` can be created from many sources.

To create a `DataFrame` from a Python list of dict objects:

```python
dogs_df = pd.DataFrame([
    {'name': 'Maisey', 'breed': 'Treeing Walker Coonhound'},
    {'name': 'Oscar', 'breed': 'German Shorthaired Pointer'},
    {'name': 'Ramsay', 'breed': 'Native American Indian Dog'},
    {'name': 'Comet', 'breed': 'Whippet'}
])
```

To create a `DataFrame` from a CSV file

```python
# If the first line contains column names ...
df = pd.read_csv('file-name.csv') # from comma-separated
df = pd.read_csv('file-name.csv', delimiter='\t') # for tab-separate

# If the first line contains data rather than column names ...
df = pd.read_csv('file-name.csv', names=['colName1', 'colName2', ...])
```

To create a `DataFrame` from an Excel spreadsheet:

```python
df = pd.read_excel('file-name.xlsx', sheet_name='name')
```

To create a `DataFrame` from a NumPy array:

```python
df = pd.DataFrame(data=numpy_array)
```

## Data Types

When pandas creates `Series` and `DataFrame` objects from Python data
it converts Python string and number types to its own types.

| Python type | pandas type |
| ----------- | ----------- |
| str         | object      |
| int         | int64       |
| float       | float64     |

For more detail, see {% aTargetBlank "https://pbpython.com/pandas_dtypes.html",
"Overview of Pandas Data Types" %}.

## Getting Data

Note that these just return data.
To print it, pass the result to the `print` function.

Start indexes (`si`) are inclusive and default to zero.
End indexes (`ei`) are exclusive and default to length.
Column name values below are abbreviated as `cn`.
Row index values below are abbreviated as `ri`.
Column index values below are abbreviated as `ci`.

| Operation                                                                     | Code                                 |
| ----------------------------------------------------------------------------- | ------------------------------------ |
| get column names                                                              | `df.columns`                         |
| get column data types                                                         | `df.dtypes`                          |
| get cell value                                                                | `df.at[ri, cn]` or `df.iloc[ri, ci]` |
| get all rows <sup>1</sup>                                                     | `df`                                 |
| get first n rows                                                              | `df.head(n)` where n defaults to 5   |
| get last n rows                                                               | `df.tail(n)` where n defaults to 5   |
| get one row                                                                   | `df.iloc[ri]`                        |
| get row range                                                                 | `df.iloc[si:ei]`                     |
| get row index values                                                          | `df.index`                           |
| get one column as a `Series` object                                           | `df.cn` or `df[cn]`                  |
| get one column of specified rows                                              | `df[col_name][si:ei]`                |
| get multiple columns in specified order                                       | `df[[cn1, cn2, ...]]`                |
| get rows matching criteria                                                    | `df.loc(criteria)`                   |
| get statistics of numeric columns<br>including count, min, max, mean, and std | `df.describe()`                      |

<sup>1</sup> By default the number of rows displayed is limited.
To remove the limit `pd.set_option('display.max_rows', None)`.

Criteria use bitwise operators for and (`&`), or (`|`), and not (`~`).
The example below gets all rows where the `name` column contains the letter "a"
and the `breed` column consists of three words.

```python
df.loc[df.name.str.contains('a') & (df.breed.str.split(' ').apply(len) == 3)]
```

To treat `Series` values as strings, add `.str`.
For example, `df.name.str`.

To iterate over all rows:

```python
for index, row in df.iterrows():
    # Use index and row (a Series object) here.
```

## Modifying Data

| Operation                                     | Code                                           |
| --------------------------------------------- | ---------------------------------------------- |
| set cell value                                | `df.at[ri, cn] = value`                        |
| add column computed from others               | `df['new_cn'] = expression`                    |
| add column that is sum of others <sup>1</sup> | `df['my sum'] = df.iloc[:, si:ei].sum(axis=1)` |
| delete columns                                | `df = df.drop(columns=['cn1', 'cn2', ...])`    |

<sup>1</sup> The first slice (colon) with no number on either side means "all rows".
The second slice specifies a column range.
Specifying `axis=1` means horizontal and `axis=0` means vertical.

Suppose we want to add a column named "name length"
that holds the length of each "name" value.

```python
df['name length'] = df['name'].apply(len)
```

Suppose we have the columns `width`, `depth`, and `height`
that represent refrigerator dimensions and we want to
add a new `volume` column.

```python
df['volume'] = df.width * df.depth * df.height
```

## Creating New DataFrames

When a new `DataFrame` is created from an existing one
the new one shares memory with original.
So changes to the underlying data affect both `DataFrame`s.

By default new `DataFrame`s retain the same index values at the original.
To reset the index values,
`new_df = new_df.reset_index(drop=True)`
or
`new_df.reset_index(drop=True, inplace=True)`.
Including `drop=True` removes the original index column.

| Operation                | Code                                                       |
| ------------------------ | ---------------------------------------------------------- |
| sort on one column       | `df.sort_values('cn', ascending=Bool)`                     |
| sort on multiple columns | `df.sort_values(['cn1', 'cn2'], ascending=[Bool1, Bool2])` |
| transpose                | `df.T`                                                     |

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
