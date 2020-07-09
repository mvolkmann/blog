---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  order: 0
  parent: D3
  title: D3 SETT Article
layout: topic-layout.njk
---

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

### Drawing Bars

Now that we know how to draw on bar,
let's draw one for each piece of data in an array.

This covers selections with update, enter, and exit sub-selections.
This covers using the `join` method.
This includes using a color scale and color indexes
to assign colors to fruits.

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
