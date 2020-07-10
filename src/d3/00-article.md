---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  order: 0
  parent: D3
  title: D3 SETT Article
layout: topic-layout.njk
---

## Introduction

D3 (<https:/>/d3js.org>) stands for Data-Driven Documents.
It renders, and re-renders, HTML elements (including `svg`) in a web browser
based on data and changes to the data.
D3 is primarily used for data visualizations such as
bar charts, pie charts, line charts, scatter plots, geographic maps, and more.
But as we will see, it has quite a bit of overlap with jQuery
and can be used for many kinds of DOM manipulations
that are not related to data visualization.

So why should I write about D3 now.
After all, it has been around for since February 2011.
Many tutorial articles already exist.
But many of those describe older versions of D3
and each major version included breaking changes.
Version 5 of D3 was released in January, 2018.
So this is my chance to write an up-to-date D3 tutorial
and try to make it very easily to understand.
Of course you will be the judge of whether I am successful.

JavaScript has been my primary programming language
for the entirety of the life of D3.
I have know of its existence for quite a while,
but have ignored it because I was more focused on other JavaScript-based
topics including jQuery, Angular, React, Vue, Svelte, and TypeScript.
It was time for me to stop ignoring D3 and dig in.

D3 was created by Mike Bostock, along with other contributors.
It is open source and uses a BSD license.

D3 uses HTML, CSS, JavaScript, the DOM, and SVG,
so knowledge of those topics is helpful.
To see examples of using D3, click the "Examples" link
near the top of <https://d3.js.org>.

There are 3 levels of D3 usage, using higher-level libraries,
copying and modifying examples, and coding from scratch.

An example of a library that uses D3 is C3 (<https://c3js.org/>).
It uses D3 under the hood to render many kinds of charts.
These charts are customizable,
but not to the extent that coding from scratch provides.
But charts can be implemented quickly this way
and the requirement to have deep knowledge of D3 is removed.
They respond well to changes in data,
including transitions from old to new values.

There are many sites that provided examples of charts implemented with D3.
Perhaps you can find an example that almost exactly what you want.
Copying and customizing the code will often require far less time
than implementing a chart from scratch.

My hope is that by the time you finish reading this article,
you will know enough about D3 to enable a final option
which is implementing an entire chart from scratch.
This provides the most flexibility, but requires the most effort.

The D3 JavaScript API uses CSS selectors
to find elements in the DOM that it modifies.
It also provides methods to append elements and text nodes,
set attributes, and modify text content.
In this respect it has overlap with jQuery functionality.

The functionality of D3 is divided into modules.
A default build includes a collection of commonly used modules.
Custom builds can omit unneeded modules and add ones not in the default build
in order to minimize the amount of code that must be downloaded to browsers.

These modules include:

- Arrays
- Axes
- Brushes
- Chords
- Collections (Objects, Maps, Sets, Nests)
- Colors
- Color Schemes
- Contours
- Dispatches
- Dragging
- Delimiter-Separated Values
- Easings
- Fetches
- Forces
- Number Formats
- Geographies
- Hierarchies
- Interpolators
- Paths
- Polygons
- Quadtrees
- Random Numbers
- Scales
- Selections
- Shapes
- Time Formats
- Time Intervals
- Timers
- Transitions
- Voronoi Diagrams
- Zooming

Let's learn D3 by example.
Many D3 tutorials start with a bar chart and so will we.
But ours will include more features than typically shown in tutorials
in order to introduce more D3 concepts.

### SVG Basics

SVG stands for Scalable Vector Graphics.
It is an XML-based syntax for specifying vector graphics
and is supported by all modern web browsers.

SVG elements include `svg`, `line`, `polygon`, `rect`, `circle`, `path`,
`text`, `image`, `g` (for group), and many more.
For our bar chart will will only need `svg`, `g`, `rect`, and `text`.

Let's get started by drawing a rectangle and putting some text on it.

![D3 SVG demo](/blog/assets/d3-svg-demo.png)

### `svg-demo.html`

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>D3 SVG Demo</title>
    <meta charset="utf-8" />
    <meta name="description" content="D3 SVG Demo" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />

    <link rel="stylesheet" href="./svg-demo.css" />

    <script src="https://d3js.org/d3.v5.min.js"></script>
    <script src="./svg-demo.js" defer></script>
  </head>
  <body>
    <svg id="chart"></svg>
  </body>
</html>
```

### `svg-demo.css`

```css
body {
  background-color: linen;
  font-family: sans-serif;
}

.bar > rect {
  stroke: black;
  stroke-width: 1;
}

.bar > text {
  fill: white;
  text-anchor: middle; /* horizontally centers */
}

#chart {
  background-color: white;
}
```

### `svg-demo.js`

```js
const HEIGHT = 300;
const WIDTH = 400;
const score = 7; // out of 10
const barHeight = HEIGHT * (score / 10);
const barWidth = 50;

const svg = d3.select('#chart').attr('width', WIDTH).attr('height', HEIGHT);

const group = svg.append('g').attr('class', 'bar');

group
  .append('rect')
  .attr('height', barHeight)
  .attr('width', barWidth)
  .attr('x', 0)
  .attr('y', HEIGHT - barHeight)
  .attr('fill', 'cornflowerblue');

group
  .append('text')
  .text(score)
  .attr('x', barWidth / 2) // center horizontally in bar
  .attr('y', HEIGHT - barHeight + 20); // just below top
```

### D3 Selection Objects

A D3 selection object encapsulates a set of DOM elements,
similar to a jQuery object.
To create one, use the d3 methods `select` and `selectAll`.
For example, `d3.select('.bar')` returns a selection object that
encapsulates the first element in the DOM with a CSS class of "bar".
`d3.selectAll('.bar')` is similar, but the returned selection object
encapsulates all the matching DOM elements.

The `select` and `selectAll` methods can also be called on a selection
to find elements that are descendants of the elements in the selection
within the DOM tree.

Here is an example that demonstrates using the `select` and `selectAll` methods.

```html
<html>
  <head>
    <title>D3 Selections</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
  </head>
  <body>
    <div class="chart" id="chart1">
      <div class="bar">Bar #1</div>
      <div class="bar">Bar #2</div>
      <div class="bar">Bar #3</div>
    </div>

    <div class="chart" id="chart2">
      <div class="bar">Bar #4</div>
      <div class="bar">Bar #5</div>
    </div>

    <script>
      const bars1 = d3.selectAll('.bar');
      console.log('bars1 =', bars1);

      const chart1 = d3.select('#chart1');
      const bars2 = chart1.selectAll('.bar');
      console.log('bars2 =', bars2);

      const charts = d3.selectAll('.chart');
      const bars3 = charts.selectAll('.bar');
      console.log('bars3 =', bars3);
    </script>
  </body>
</html>
```

Here is the output from the `console.log` calls from the DevTools console.
![selection objects in console](/blog/assets/d3-selections-in-console.png)

Each selection object has the following properties:

- `_parents`
  This is an array of the DOM elements that were searched
  When the `select` and `selectAll` methods are called on the `d3` object,
  this array will contain only the `html` element of the current document.
  When these methods are called on an existing selection,
  this array will contain all the DOM elements in that selection.
- `_groups`
  This is an array of DOM NodeList objects, one per parent element.
  These NodeList objects contain the matching DOM elements.

In the `bars1` selection, `_parents` contains only the `html` element,
indicating that the entire document was searched.
`_groups` holds a single `NodeList` that
contains every DOM element with a CSS class of "bar".

In the `bars2` selection, `_parents`
contains only the element with an id of "chart1".
This happens because the search was performed on
a selection that only contains that element.
`_groups` holds a single `NodeList` that
only contains the DOM elements with a CSS class of "bar"
that are descendants of the element with an id of "chart1".

In the `bars3` selection, `_parents`
contains the two elements with a CSS class of "chart"
because the search was performed on a selection
that only contains those elements.
`_groups` holds two `NodeList` objects, one for each parent element.
Each of these contains the DOM elements with a CSS class of "bar"
that are descendants of their respective parent element.

### The `data` method

Selection objects have a `data` method that associates data in an array
with the DOM elements in the selection.
It does this by adding a `_data_` property to them.

The `data` method returns a new selection that contains three sub-selections
referred to as "update", "enter", and "exit".
"update" contains all the DOM elements in the selection
that CAN be associated with data value.
"enter" contains a placeholder for each DOM element
that must be created in order to associated a data value.
"exit" contains all the DOM elements in the selection
for which no data will be assigned.
Typically these DOM elements are removed.

Let's look at three scenarios.

#### Scenario #1

Often the first time the `data` method is called,
the selection does not yet contain an DOM elements.
So update and exit will be empty, and enter will contain
a placeholder for each of the data values.
These placeholder objects will contain a `_data_` property
whose value is one of the values from the data array.
The enter sub-selection can then be used to create
a new DOM element for each placeholder object.
We will see how to do this later.

For example:

```js
const values = [7, 13, 2];
const bars = d3.selectAll('.bar').data(values);
```

#### Scenario #2

The `data` method can be called again later on a new selection
using a new data array.
Suppose that in the previous call to `data` there were 3 data values
and in this call there are 5 data values.
The update sub-selection will contain all three of the existing DOM elements,
the enter sub-selection will contain two placeholders,
and the exit sub-selection will be empty.
Like in scenario #1, these placeholder objects
will have an associated data value.
The enter sub-selection can then be used to create
a new DOM element for each placeholder object.

#### Scenario #3

Suppose that in the previous run there were 5 data values
and in this run there are 3 data values.
The update sub-selection will contain three of the five existing DOM elements,
the enter sub-selection will be empty, and
the exit sub-selection will contain two DOM elements that can be removed.

Selection objects support many methods,
some which act on all the DOM elements they encapsulate.
These methods take a function that is invoked
once for each encapsulated DOM element.
The function is passed the value of the DOM element _data_ property
and the index of the DOM element within the selection.
Just like in jQuery, it is not an error to call such a method on a
selection that is empty, meaning it doesn't encapsulate any DOM elements.
TODO: Add an example.

Selection objects are immutable, meaning
the set of DOM elements they encapsulate cannot be changed.
However, there are many methods on selection objects that return a new selection,
including `map` and `filter`.

The selection object `join` method ...
Is this method supposed to be used instead of `data` or in conjunction with it?

### Drawing Bars

Now that we know how to draw on bar,
let's draw one for each piece of data in an array.
We will create a bar chart like the following:

![D3 Bar Chart](/blog/assets/bar-chart.png)

The "Update" button generates new, random data
and updates the bars.
This is implemented by the files
`index.html`, `bar-chart.css`, and `bar-chart.js`
which are shown below.

This code gives us a good base for adding features.
There are many D3 concepts to cover here.
See comments in the code for details.

### `index.html`

```html
<html>
  <head>
    <title>D3 Bar Chart</title>
    <meta charset="utf-8" />
    <meta name="description" content="D3 Bar Chart" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />

    <link rel="stylesheet" href="./bar-chart.css" />

    <script src="https://d3js.org/d3.v5.min.js"></script>
    <script src="./bar-chart.js" defer></script>
  </head>
  <body>
    <!-- The bars will be added in this svg element. -->
    <svg id="chart"></svg>

    <div>
      <!-- Render a new version of the chart when this is pressed. -->
      <button onclick="updateData()">Update</button>
    </div>
  </body>
</html>
```

### `bar-chart.css`

```css
body {
  background-color: linen;
  font-family: sans-serif;
}

.bar > rect {
  stroke: black;
  stroke-width: 1;
}

button {
  margin-top: 1rem;
}

#chart {
  background-color: white;
}
```

### `bar-chart.js`

```js
// Later these will be adjusted to make room
// for a vertical and horizontal axis.
const BOTTOM_PADDING = 10;
const LEFT_PADDING = 10;
const RIGHT_PADDING = 10;
const TOP_PADDING = 10;

// Full size of the svg element.
const HEIGHT = 300;
const WIDTH = 400;

// Size that can be used for the bars.
const usableHeight = HEIGHT - TOP_PADDING - BOTTOM_PADDING;
const usableWidth = WIDTH - LEFT_PADDING - RIGHT_PADDING;

// Random data will be selected from this array.
const allData = [
  {name: 'apple', colorIndex: 1},
  {name: 'banana', colorIndex: 2},
  {name: 'cherry', colorIndex: 3},
  {name: 'date', colorIndex: 4},
  {name: 'grape', colorIndex: 5},
  {name: 'mango', colorIndex: 6},
  {name: 'peach', colorIndex: 7},
  {name: 'raspberry', colorIndex: 8},
  {name: 'strawberry', colorIndex: 9},
  {name: 'tangerine', colorIndex: 10},
  {name: 'watermelon', colorIndex: 11}
];

let barPadding, barWidth, xScale, yScale;

// This is used to select bar colors based on their score.
const colorScale = d3.scaleOrdinal(d3.schemePaired); // 12 colors

// This returns a random integer from 1 to max inclusive.
const random = max => Math.floor(Math.random() * max + 1);

// This returns an array of objects taken from allData.
// A "score" property with a random value from 1 to 10
// is added to each object.
function getRandomData() {
  const count = random(allData.length);
  const shuffled = allData.sort(() => 0.5 - Math.random());
  const data = shuffled.slice(0, count);
  data.sort((f1, f2) => f1.name.localeCompare(f2.name));
  for (const item of data) {
    item.score = random(10);
  }
  return data;
}

// This updates the attributes of an SVG rect element
// that represents a bar.
function updateRect(rect) {
  rect
    // Each fruit will keep the same color as its score changes.
    .attr('fill', d => colorScale(d.colorIndex))
    .attr('width', barWidth - barPadding * 2)
    .attr('height', d => usableHeight - yScale(d.score))
    .attr('x', barPadding)
    .attr('y', d => TOP_PADDING + yScale(d.score));
}

// This updates the bar chart with random data.
function updateData() {
  const data = getRandomData();

  // Calculate padding on sides of bars based on # of bars.
  barPadding = Math.ceil(30 / data.length);

  // Calculate the width of each bar based on # of bars.
  barWidth = usableWidth / data.length;

  // Create a scale to map data index values to x coordinates.
  // This is a function that takes a value in the "domain"
  // and returns a value in the "range".
  xScale = d3
    .scaleLinear()
    .domain([0, data.length])
    .range([LEFT_PADDING, LEFT_PADDING + usableWidth]);

  // Create a scale to map data score values to y coordinates.
  // The range is flipped to account for
  // the SVG origin being in the upper left corner.
  // Like xScale, this is a function that takes a value in the "domain"
  // and returns a value in the "range".
  const max = d3.max(data, d => d.score);
  yScale = d3.scaleLinear().domain([0, max]).range([usableHeight, 0]);

  // Create a D3 selection object that represents the svg element
  // and set the size of the svg element.
  const svg = d3.select('#chart').attr('width', WIDTH).attr('height', HEIGHT);

  // This is the most critical part to understand!
  //
  // The data method ...
  //
  // The join method ...
  const groups = svg
    .selectAll('.bar')
    .data(data, d => d.name)
    .join(
      enter => {
        // Create a new SVG group element for each placeholder
        // to represent a new bar.
        const groups = enter.append('g').attr('class', 'bar');

        // Create a new SVG rect element for each group.
        groups
          .append('rect')
          .attr('height', 0)
          .attr('y', TOP_PADDING + usableHeight);

        // Create a new SVG text element for each group.
        groups.append('text').attr('y', TOP_PADDING + usableHeight);

        return groups;
      },
      update => update,
      exit => {
        // Shrink the height of the exiting rects gradually
        // and then remove them them.
        exit
          .select('rect')
          .attr('height', 0)
          .attr('y', TOP_PADDING + usableHeight)
          .on('end', () => exit.remove());
      }
    );

  // The join method call above returns a selection that combines
  // the update and enter sub-selections into its update selection.
  // This allows operations needed on elements in both
  // to be performed on the new selection.

  // Translate the groups for each bar to their
  // appropriate x coordinate based on its index.
  groups.call(group =>
    group.attr('transform', (_, i) => `translate(${xScale(i)}, 0)`)
  );

  // Update all the rects using their newly associated data.
  groups.select('rect').call(updateRect);
}

// Render the first version of the chart.
updateData();
```

## Adding Y Axis

This covers `d3.max`, scales, and axis methods.
This includes adding left and top padding to the chart.

## Adding X Axis

This includes translating and rotating axis labels using the
CSS `transform` property with the `translate` and `rotate` functions.
This includes adding bottom and right padding to the chart.

## Adding Text on Bars

This includes selecting a text color based on the bar color
using relative luminance.
See the `getTextColor` function.
This includes horizontal centering with the
CSS `text-anchor` property set to `middle`.

## Responding to Data Updates

This includes generating random data.
See the `random` and `getRandomData` functions.

## Adding Animation

This includes using `transition`, `duration`, and `easing` methods.
It also explains use of the `myTransition` function.
