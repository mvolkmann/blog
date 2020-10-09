## Joint plot

This plots two variables with bivariate and univariate graphs.
It shows a dot for each car at year/mpg intersection and a linear regression line.
The histogram at the top indicates the number of cars in each model year range.
The histogram on the right indicates the number of cars in each mpg range.
We see from this that the most common mpg range is around 19 mpg.

Setting the `kind` argument to `reg` causes the
linear regression line to be computed and rendered.

```python
sns.jointplot(data=df, x='model_year', y='mpg', kind='reg')
```

    <seaborn.axisgrid.JointGrid at 0x7ff54033c760>

![png](output_12_1.png)

# KDE plot

From the docs, "A kernel density estimate (KDE) plot is a method for visualizing the
distribution of observations in a dataset, analagous to a histogram.
KDE represents the data using a continuous probability density curve in one or more dimensions."
This gives an indication of the number of vehicles in the dataset for each MPG value.

```python
sns.kdeplot(df['mpg'])
```

    <matplotlib.axes._subplots.AxesSubplot at 0x7ff52008ae80>

![png](output_14_1.png)

## Style

From the docs, the `set_style` method "affects things like the color of the axes,
whether a grid is enabled by default, and other aesthetic elements."
Call this before generating a plot.
To set colors, pass it one of "white", "dark", "whitegrid", or "darkgrid".
To add tick marks to the axes, pass it "ticks".
It seems that it is not possible to set colors AND add tick marks
because that requires two calls and the second just overrides the first.

```python
sns.set_style('darkgrid')
#sns.set_style('ticks')
sns.displot(df['mpg'], bins=20, kde=True)
```

    <seaborn.axisgrid.FacetGrid at 0x7ff5393a4a00>

![png](output_16_1.png)

## Context

From the docs, the `set_context` method "affects things like
the size of the labels, lines, and other elements of the plot,
but not the overall style" of the most recent plot.
The default context is “notebook”. Other contexts are “paper”, “talk”, and “poster”,
which scale values used by "notebook" by 0.8, 1.3, and 1.6, respectively.

Explicitly setting the context to "notebook" makes everything much larger than expected!

## Removing spines

Call `sns.despine()` after creating a plot to
remove the solid lines and tick marks from specific sides,
passing it boolean arguments that indicate which sides to remove.

When `bottom` is set to `True` the top spine is also removed unless `top` is set to `False`.
Similarly, when `left` is set to `True` the right spine is also removed unless `right` is set to `False`.

For example, in the previous example we can display right and top spines
instead of left and bottom spines as follows.

```python
sns.kdeplot(df['mpg'])
sns.despine(left=True, right=False, bottom=True, top=False)
```

![png](output_19_0.png)

## Color palettes

By default, Seaborn supports six variations of matplotlib palettes.
These are "deep", "muted", "pastel", "bright", "dark", and "colorblind".
The colors in each palette can be viewed at <https://seaborn.pydata.org/tutorial/color_palettes.html>.
One of these is used in the next example.

## Pair plot

From the docs, "By default, this function will create a grid of Axes
such that each numeric variable in data will by shared
across the y-axes across a single row and the x-axes across a single column.
The diagonal plots are treated differently:
a univariate distribution plot is drawn to show
the marginal distribution of the data in each column."
Wow, that's a lot to take in!

The "vars" argument limits the variables that are plotted.

The "palette" argument specifies a color palette to use.

The "hue" argument specifies a column name to be treated as a category
for the purpose of assigning colors from the palette.

This takes a long time to complete (~15 seconds) because it generates many plots.

Compare this to PairGrid plot described later.

```python
sns.set_style('white')
sns.pairplot(df, vars=['cylinders', 'horsepower', 'model_year', 'mpg'], palette="pastel", hue="model_year")
```

    /Users/mark/opt/anaconda3/lib/python3.8/site-packages/seaborn/distributions.py:305: UserWarning: Dataset has 0 variance; skipping density estimate.
      warnings.warn(msg, UserWarning)





    <seaborn.axisgrid.PairGrid at 0x7ff54037e580>

![png](output_22_2.png)

## Rug plot

This plots "marginal distributions by drawing ticks along the x and y axes."
It seems fairly useless compared to the other supported plots.

```python
sns.rugplot(df['mpg'])
```

    <matplotlib.axes._subplots.AxesSubplot at 0x7ff5201e3e20>

![png](output_24_1.png)

## Bar plot

From the docs, "A bar plot represents an estimate of central tendency
for a numeric variable with the height of each rectangle and
provides some indication of the uncertainty around that estimate using error bars."

For example, the plot below shows the average MPG value for cars in each model year
and the black vertical line at the top of each bar
shows the min and max values in that model year.

To change the calculation of the heights, add the `estimator` argument,
set to the function that will compute the value.
For example, to plot the maximum values in each model year,
add `estimator=np.max`.

```python
sns.barplot(data=df, x='model_year', y='mpg')
```

    <matplotlib.axes._subplots.AxesSubplot at 0x7ff580688b80>

![png](output_26_1.png)

## Count plot

This produces a bar chart that shows the rows counts
for each unique value of a given column.

```python
sns.countplot(data=df, x='model_year')
```

    <matplotlib.axes._subplots.AxesSubplot at 0x7ff58029d670>

![png](output_28_1.png)

## Box plot (a.k.a. box and wisker)

From the docs, this plot "shows the distribution of quantitative data
in a way that facilitates comparisons between variables
or across levels of a categorical variable.
The box shows the quartiles of the dataset
while the whiskers extend to show the rest of the distribution,
except for points that are determined to be “outliers”
using a method that is a function of the inter-quartile range."

- The horizonal line across the boxes represents the median value, not the mean.
- The box extends one standard deviation above and below the median.
- The horizonal line at the top of the top "whisker" represents the maximum value.
- The horizonal line at the bottom of the bottom "whisker" represents the minimum value.
- The small black diamonds represent outlier data.

```python
cylinders = [4, 6, 8] # not interested in 3 or 5 cylinder cars
subset_df = df.loc[df['cylinders'].isin(cylinders)]
year_71_df = subset_df.loc[subset_df['model_year'] == 71]
print('For cars in 1971, min =', year_71_df['mpg'].min(),
      ', max =', year_71_df['mpg'].max(),
      ', mean =', year_71_df['mpg'].mean(),
      ', and median =', year_71_df['mpg'].median())

plt.figure(figsize=(16,8))
# This will render one box for each hue value within each x value.
# Statistics are gathered from the y values.
ax = sns.boxplot(data=subset_df, x='model_year', y='mpg', hue='cylinders')

# Display legend outside of the plot.  These markers are not helpful!
#ax.legend(list(df.model_year.unique()), bbox_to_anchor=(1, 1.02))
```

    For cars in 1971, min = 12.0 , max = 35.0 , mean = 21.25 , and median = 19.0

![png](output_30_1.png)

## Strip plot

From the docs, this draws "a scatterplot where one variable is categorical."

In the example below, the categorical variable "model_year".

- Add `dodge=True` to place the dots for each `hue` value in its own column.
- Add `jitter=True` to spread the dots horizontally a bit farther to reduce overlap.

```python
plt.figure(figsize=(16,8))
# Setting jitter to True spreads the data points horizontally a bit farther.
sns.stripplot(data=df, x='model_year', y='mpg', hue='cylinders')
plt.xlabel('Model Year')
plt.ylabel('Miles Per Gallon')
```

    Text(0, 0.5, 'Miles Per Gallon')

![png](output_32_1.png)

## Colormaps

For options, see <https://matplotlib.org/3.1.0/tutorials/colors/colormaps.html>.
To choose one, add `palette` argument set to a colormap name.
For example, here is the previous plot using the "hsv" colormap.

```python
plt.figure(figsize=(16,8))
sns.stripplot(data=df, x='model_year', y='mpg', hue='cylinders', palette='hsv')
```

    <matplotlib.axes._subplots.AxesSubplot at 0x7ff560167790>

![png](output_34_1.png)

## Legend location

To add a legend to a plot, call `plt.legend()`.
By default, matplotlib takes its best quess on where to place the legend.
To override this with a specific location, add the `loc` argument set to a string or a number.

| String | Code |
| 'best' | 0 |
| 'upper right' | 1 |
| 'upper left' | 2 |
| 'lower left' | 3 |
| 'lower right' | 4 |
| 'right' | 5 |
| 'center left' | 6 |
| 'center right' | 7 |
| 'lower center' | 8 |
| 'upper center' | 9 |
| 'center' | 10 |

All of these options place the legend inside the plot.
To display the legend outside of the plot, add the `bbox_to_anchor` argument
set to a tuple that specifies its x and y location
with percentages of the plot width and height.
For example, `bbox_to_anchor=(1, 1)` places the upper-left corner
of the legend at the top-right corner of the plot.

## Swarm plot

From the docs, this draws "a categorical scatterplot with non-overlapping points."
It looks nearly identical to a strip plot, but
the points are spread more horizontally so they do not overlap.

```python
plt.figure(figsize=(16,8))
sns.swarmplot(data=df, x='model_year', y='mpg', hue='cylinders', palette='hsv')
```

    <matplotlib.axes._subplots.AxesSubplot at 0x7ff53243dcd0>

![png](output_37_1.png)

## Heatmap plot from correlation data

A heatmap shows the degree of correlation between column values.

Create a matrix `Dataframe` from the original one that contains the correlation values
and pass this to the `heatmap` method.
Set the `annot` argument to `True` to display correlation values in the cells.
Every column has strong correlation with itself, receiving a value of 1.
Cells with darker colors indicate strong correlation between
the column name to the left of the cell and the column name below the cell.

The example below shows the following:

- The value that most affects `mpg` is `model_year`.
- There is a strong correlation between `displacement` and `cylinders`.
- There is a weak correlation between `horsepower` and `acceleration`.

```python
matrix = df.corr()
print(matrix)
sns.heatmap(matrix, annot=True, cmap='YlOrRd', linecolor='white', linewidth=1)
```

                       mpg  cylinders  displacement  horsepower    weight  \
    mpg           1.000000  -0.775396     -0.804203   -0.778427 -0.831741
    cylinders    -0.775396   1.000000      0.950721    0.842983  0.896017
    displacement -0.804203   0.950721      1.000000    0.897257  0.932824
    horsepower   -0.778427   0.842983      0.897257    1.000000  0.864538
    weight       -0.831741   0.896017      0.932824    0.864538  1.000000
    acceleration  0.420289  -0.505419     -0.543684   -0.689196 -0.417457
    model_year    0.579267  -0.348746     -0.370164   -0.416361 -0.306564

                  acceleration  model_year
    mpg               0.420289    0.579267
    cylinders        -0.505419   -0.348746
    displacement     -0.543684   -0.370164
    horsepower       -0.689196   -0.416361
    weight           -0.417457   -0.306564
    acceleration      1.000000    0.288137
    model_year        0.288137    1.000000





    <matplotlib.axes._subplots.AxesSubplot at 0x7ff580a2ea00>

![png](output_39_2.png)

## Heatmap plot from pivot table

This approach to creating a heatmap is useful when the correlation we wish to show is between specific values of a given column.
For example, we can correlate `model_year` and `cylinders` to `mpg`.
Note that some cells have no value because there are no cars in the dataset
for that combination of `model_year` and `cylinders`.
The example below shows that in this dataset
the highest `mpg` comes from 1980 cars with 5 cylinders.

```python
matrix = df.pivot_table(columns='model_year', index='cylinders', values='mpg')
sns.heatmap(matrix, annot=True, cmap='YlOrRd')
```

    <matplotlib.axes._subplots.AxesSubplot at 0x7ff550090df0>

![png](output_41_1.png)

## Cluster map

From the docs, this plots "a matrix dataset as a hierarchically-clustered heatmap."
It is very similar to the "Heat map from correlation data" example above,
but it clusters like data points by reordering the rows and columns.

The meaning of the grouping brackets on the left and top sides of the plot
are not clear to me.
I know they represent clusters of data, but what does that mean?

```python
matrix = df.corr()
sns.clustermap(matrix, annot=True, cmap='YlOrRd')
```

    <seaborn.matrix.ClusterGrid at 0x7ff580a7ad90>

![png](output_43_1.png)

## PairGrid plot

The creates many plots of a specific kind for each combination of two columns.
For example, we can choose to create scatter plots where
the color of each dot is determined by the `mpg` value.
Focusing on one of the generated plots, the one in the lower-left shows
the `mpg` using colors for combinations of `cylinder` and `model_year` values.
It shows that for a given number of cylinders, the mpg average
has tended to increase for newer model years.

To limit the columns that are mapped to columns, add the `x_vars` argument whose value is a list of column names.

To limit the columns that are mapped to rows, add the `y_vars` argument whose value is a list of column names.

Compare this to Pair plot described earlier.

```python
mpg_grid = sns.PairGrid(
    df, hue='mpg', palette='YlOrRd',
    x_vars=['cylinders', 'horsepower', 'model_year'],
    y_vars=['horsepower', 'weight', 'acceleration'])
mpg_grid.map(plt.scatter)
```

    <seaborn.axisgrid.PairGrid at 0x7ff532e9a6a0>

![png](output_45_1.png)

## FacetGrid plot

From the docs, this is a "multi-plot grid for plotting conditional relationships."

In the example below there is one plot for each combination of `cylinders` and `model_year`.
Each plot is a scatter plot where the x-axis is `horsepower` and the y-axis is mpg`.

```python
# Get the subset of the data that excludes 3 and 5 cylinder cars
# and only includes model years in the 80's.
cylinders = [4, 6, 8]
subset_df = df.loc[df['cylinders'].isin(cylinders) & (df['model_year'] >= 80)]

fg = sns.FacetGrid(subset_df, col='cylinders', row='model_year', hue='mpg')
fg.map(sns.scatterplot, 'horsepower', 'mpg')
```

    <seaborn.axisgrid.FacetGrid at 0x7ff5201cd880>

![png](output_47_1.png)

## Regression plot

From the docs, this plots "data and regression model fits across a FacetGrid."

In the example below we plot a point for each car
where the x-axis is `model_year` and the y-axis is `mpg`.
The color of each point is based its `cylinders` value.
Each regression line shows the change in `mpg` for a given `cylinders` value.
This shows that `mpg` has improved more rapidly for 4 cylinder cars
than for cars with more cylinders.

```python
cylinders = [4, 6, 8] # not interested in 3 or 5 cylinder cars
subset_df = df.loc[df['cylinders'].isin(cylinders)]
sns.lmplot(data=subset_df, x='model_year', y='mpg', hue='cylinders')
```

    <seaborn.axisgrid.FacetGrid at 0x7ff539091f40>

![png](output_49_1.png)

To show separate plots for each unique value of a given column,
add the `col` argument set to that column name.

```python
sns.lmplot(data=subset_df, x='model_year', y='mpg', hue='cylinders', col='cylinders')
```

    <seaborn.axisgrid.FacetGrid at 0x7ff53312ddc0>

![png](output_51_1.png)
