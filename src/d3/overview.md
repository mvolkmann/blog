---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  key: D3 Overview
  parent: D3
  title: Overview
layout: topic-layout.njk
---

D3 stands for Data-Driven Documents.
It renders, and re-renders, HTML elements (including `svg`)
based on data and changes to the data.
The main website is at <https://d3js.org>.
To see examples, click the "Examples" link near the top.

D3 uses HTML, CSS, JavaScript, the DOM, and SVG.

The D3 JavaScript API uses CSS selectors
to find elements in the DOM that it modifies.
It also provides methods to append elements and text nodes,
set attributes, and modify text content.
In this respect it has overlap with jQuery functionality.

## Basic Example

```html
<!DOCTYPE html>
<html>
  <head>
    <title>My D3 Demo</title>
    <script src="http://d3js.org/d3.v3.min.js"></script>
  </head>
  <body>
    <p>This is a paragraph.</p>

    <script>
      // To change the text in the p element above ...
      d3.select('p').text('Hello, World!');

      // To add a p element including text to the body ...
      d3.select('body').append('p').text('Hello, World!');
    </script>
  </body>
</html>
```

## API Basics

To select the first occurrence of an element with a given name
inside a parent element  
<code>.select(<i>childElementName</i>)`</code>

To select all occurrences of elements with a given name
inside a parent element  
<code>.selectAll(<i>childElementName</i>)`</code>

To append a new element with a given name to a parent element  
<code>.append(<i>elementName</i>)`</code>

To set an attribute on an element  
<code>.attr(<i>property</i>, <i>name</i>)`</code>

To get the value of an attribute on an element  
<code>.attr(<i>property</i>>)`</code>

For example, to translate an element such as an SVG `g`(for group),  
<code>.attr('transform', 'translate(100, 50))`</code>

To set a CSS style property on an element  
<code>.style(<i>property</i>, <i>name</i>)</code>

To add a CSS class to an element  
<code>.classed(<i>className</i>, true)</code>

To set the text content of an element  
<code>.text(<i>textValue</i>, true)</code>

To set the HTML content of an element  
<code>.html(<i>htmlString</i>, true)</code>

For example:

![DOM Basics](/blog/assets/d3-dom-basics.png)

```html
<html>
  <head>
    <title>D3 DOM basics</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>

    <style>
      .bordered {
        border: solid orange 3px;
        border-radius: 10px;
        padding: 0.5rem;
      }

      button {
        font-size: 2rem;
      }

      button:disabled {
        border-color: red;
      }
    </style>
  </head>
  <body>
    <script>
      const lockUnicode = '&#x1F512;';
      d3.select('body')
        .append('button')
        .attr('disabled', 'true')
        .classed('bordered', true)
        .style('background-color', 'cornflowerblue')
        .style('color', 'white')
        .html('<i>Hello, World!</i> ' + lockUnicode);
    </script>
  </body>
</html>
```

## data method

The data method "joins" data values to DOM elements.
For example, the following HTML contains an ordered list
with three empty list items.
The variable `letters` is an array holding three letters.
We first select all the DOM list items.
Then we use the `data` method to set their text
to the values in the `letters` array.

```html
<html>
  <head>
    <title>D3 data basic demo</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
  </head>
  <body>
    <ol>
      <li></li>
      <li></li>
      <li></li>
    </ol>

    <script>
      const letters = ['a', 'b', 'c'];

      const lis = d3
        .select('ol')
        .selectAll('li')
        .data(letters)
        .text(d => d);
    </script>
  </body>
</html>
```

The `data` method associates data values
with the DOM elements to which they are added
by setting the property `__data__` to the value.
For example, the first `li` DOM element above
will have a `__data__` property set to `a`.
To see this in the browser DevTools, enter the following in the Console:
`document.querySelector('li').__data__`.

There are three possibilities to consider when the `data` method
joins data values to DOM elements.

1. The number of data values is equal to the number target DOM elements.
1. There are more data values, so new target DOM elements to be added.
1. There are fewer data values, so some target DOM elements need to be removed.

The `data` method supports the last two possibilities by
returning what is referred to as an "update selection".
This holds two collections referred to as
the "enter selection" and "exit selection".
If the number of data values is
greater than the number of targeted DOM elements,
the excess data values are placed in the enter selection.
If the number of targeted DOM elements is
greater than the number of data values,
the excess DOM elements are placed in the "exit selection".

## enter and exit methods

The `enter` and `exit` methods are useful when rendering data where
the target DOM elements do not exist yet
or the number of data values can change,
perhaps in response to user interactions.

The `enter` and `exit` methods can be called on an update selection
returned by the `data` method.
The `enter` method iterates over its enter selection.
The `exit` method iterates over its exit selection.

The example below illustrates this.
The variable `letters` holds an array of letters, 'a' through 'e'.
These are rendered as list items in an ordered list
by calling the `populateList` function.
The ordered list begins with two empty list items.
These list items have assigned CSS class names so we can
identify them when inspecting the DOM.

Since we initially have more data values (5) than list items (2),
new list items need to be created.
After the initial data values are used to populate the existing list items,
the `enter` method iterates over the remaining data values.
We follow this with a call to append a new list item and then set its text.
Note that the text is the uppercase version of the letter
so it's easy to see which list items were populated using `enter`.

Pressing the "Remove One" button invokes the `removeOne` function.
This removes the last element from the `letters` array
and calls the `populateList` function again.
Now there are five list item elements,
so there is no work for the `enter` method to do.
We can tell that the `enter` method isn't responsible for
any of the current list item values
because all of them are lowercase letters now.

The `exit` method iterates over the unused list item DOM elements.
Since we went from having five data values to four,
there is now one unused list item DOM element.
We follow the call to `exit` with a call to `remove`
to remove the excess DOM elements.
Without this line of code, the list item DOM element
for 'e' would still be present.

This process repeats each time the "Remove One" button is pressed,
removing one more value for the end of the `letters` array
and removing one more list item element from the DOM.

Pressing the "Add One" button invokes the `addOne` function.
This adds an `x` to the end of the `letters` array
and calls the `populateList` function again.
Now there is work for the `enter` method to do
because there are not enough list item DOM elements
to hold all the values in the `letters` array.
We can tell that the `enter` method is responsible for
adding the new value because it will be uppercase.

In typical usage, initially there are no existing target DOM elements
to hold data values and they all need to be created using the `enter` method.

The `exit` method is only needed if the number of data values
might be reduced after they are initially rendered.
If the `exit` method is not used, excess DOM elements will be retained.

```html
<html>
  <head>
    <title>D3 enter/exit demo</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
  </head>
  <body>
    <ol>
      <li class="first"></li>
      <li class="second"></li>
    </ol>

    <button onclick="addOne()">Add One</button>
    <button onclick="removeOne()">Remove One</button>

    <script>
      const letters = ['a', 'b', 'c', 'd', 'e'];

      function populateList() {
        const lis = d3
          .select('ol')
          .selectAll('li')
          .data(letters)
          .text(d => d);

        lis
          .enter()
          .append('li')
          .text(d => d.toUpperCase());

        lis.exit().remove();
      }

      function addOne() {
        letters.push('x');
        populateList();
      }

      function removeOne() {
        letters.pop();
        populateList();
      }

      populateList();
    </script>
  </body>
</html>
```

For another explanation of the `data`, `enter`, and `exit` methods,
see <https://medium.com/@c_behrens/enter-update-exit-6cafc6014c36>,

## Basic SVG Drawing

![SVG Basics](/blog/assets/d3-svg-basics.png)

```html
<html>
  <head>
    <title>D3 SVG basics</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
  </head>
  <body>
    <script>
      const svg = d3
        .select('body')
        .append('svg')
        .attr('width', 400)
        .attr('height', 300);

      svg
        .append('rect')
        .attr('x', 10)
        .attr('y', 10)
        .attr('width', 100)
        .attr('height', 200)
        .attr('fill', 'red')
        .attr('stroke', 'green')
        .attr('stroke-width', 5);

      svg
        .append('circle')
        .attr('cx', 100)
        .attr('cy', 100)
        .attr('r', 75)
        .attr('fill', 'yellow')
        .attr('stroke', 'blue')
        .attr('stroke-width', 5);

      svg
        .append('line')
        .attr('x1', 100)
        .attr('y1', 100)
        .attr('x2', 300)
        .attr('y2', 200)
        .attr('stroke', 'purple')
        .attr('stroke-linecap', 'round') // other values are 'butt' and 'square'
        .attr('stroke-width', 5);
    </script>
  </body>
</html>
```

## Bar Charts

It is common to need to "scale" data values to fit in a range of values
that correspond to the width or height of an element such as `svg`.
D3 provides several methods that create a function for doing this.
The most commonly used is the `scaleLinear` method.

The `scaleLinear` method returns an object
that supports `domain` and `range` methods.
The domain represents the bounds (min to max)
of data values that need to be scaled.
The range represents the bounds to which they will be mapped.

For example, suppose our data is an array of objects that have
the properties `countryName` and `population`.
We want to represent values as low as zero
and as high as the highest population value.

To determine the minimum and maximum data values,
we can use the `min` and `max` methods.
These take an array of data and an optional function
the will be passed a data element and returns a value
to be considered for determining the minimum or maximum value.
For example, we can determine the maximum population in our data
with the following:

```js
const maxPopulation = d3.max(data, d => d.population);
```

If we want to show the values as horizontal bars in an SVG that
has a width of 800, the following code creates a function to do this:

```js
const myScale = d3.scaleLinear().domain([0, maxPopulation]).range([0, 800]);
```

The `myScale` function we have created
takes a number between 0 and `maxPopulation`
and it returns a number between 0 and 800.

When rendering SVG `rect` elements for each of the bars,
we can use this function as follows to set their widths:

```js
  .attr('width', d => myScale(d.population))
```

Here is HTML and JavaScript code for creating a simple bar chart
using D3 and SVG.

![SVG bar chart](/blog/assets/d3-svg-bar-chart.png)

### index.html

```html
<html>
  <head>
    <title>D3 SVG Bar Chart</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
    <script src="bar-chart.js" defer></script>
    <style>
      body {
        font-family: sans-serif;
      }

      rect {
        fill: cornflowerblue;
      }

      text {
        fill: white;
      }
    </style>
  </head>
  <body></body>
</html>
```

### bar-chart.js

```js
const barHeight = 20;
const barMargin = 4; // vertical spacing between
const barTotal = barHeight + barMargin;
const players = [
  {name: 'Mark', score: 20},
  {name: 'Tami', score: 40},
  {name: 'Amanda', score: 50},
  {name: 'Jeremy', score: 70}
];
const svgWidth = 400;
const svgHeight = 300;

// Get the highest score.
const highScore = d3.max(players, player => player.score);

// Create a linear scale that maps values from zero to the maximum score
// to values from zero to the width of the SVG.
const widthScale = d3.scaleLinear().domain([0, highScore]).range([0, svgWidth]);

// Create an SVG element.
const svg = d3
  .select('body')
  .append('svg')
  .attr('width', svgWidth)
  .attr('height', svgHeight);

// Create a selection containing one SVG group for each data value
// that are translated in the y-direction so they are visually separated.
const barGroups = svg
  .selectAll('g')
  .data(players)
  .enter()
  .append('g')
  .attr('transform', (_, i) => `translate(0, ${i * barTotal})`);

// Create a rect for each data value.
barGroups
  .append('rect')
  .attr('width', player => widthScale(player.score))
  .attr('height', barHeight);

// Create text for each data value that displays a player name.
barGroups
  .append('text')
  .text(player => player.name)
  .attr('x', 6) // at beginning of bar
  .attr('y', barTotal / 2 + 3); // centered vertically

// Create text for each data value that displays a player score.
barGroups
  .append('text')
  .text(player => player.score)
  .attr('x', player => widthScale(player.score) - 24) // at end of bar
  .attr('y', barTotal / 2 + 3); // centered vertically
```

## Adding Axes

The D3 methods to create axes do so by mapping a data range
to a width to a pixel range.
They draw an axis containing tick marks and values.
But they do not position the axes relative to a chart.
That part is up to you.

There are four methods for creating an axis.
They are `axisBottom`, `axisLeft`, `axisTop`, and `axisRight`.
The differ in the orientation of the axis (horizontal or vertical)
and the side of the axis on where tick marks appear.

The `ticks` method specifies the number of ticks to display.
They will be spaced evenly across the axis.

The `tickFormat` method can be set to a function that takes a tick index
and returns the label to render for that tick.
Pass an empty string to render an empty tick label for all of them.

The `tickSize` method specifies the length of each tick.
It defaults to 6.
Set it to a negative number to draw lines across the chart.
For example, `.tickSize(-usableHeight)`.

The `tickPadding` method specifies the space between each tick
and corresponding label. It defaults to 3.

The `tickValues` method takes an array of labels to be rendered for the ticks.
By default the data value is rendered.

![SVG bar chart with axes](/blog/assets/d3-svg-bar-chart-with-axes.png)

### index.html with axes

```html
<html>
  <head>
    <title>D3 SVG Bar Chart</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
    <script src="bar-chart.js" defer></script>
    <style>
      .bar text {
        fill: white;
        pointer-events: none;
      }

      body {
        font-family: sans-serif;
      }

      .major-x-axis > .tick > line {
        stroke-width: 2;
      }

      .minor-x-axis {
        color: orange;
      }

      rect {
        fill: cornflowerblue;
      }

      svg {
        background-color: linen;
      }

      .tooltip {
        position: absolute;
        background: pink;
        border: solid gray 1px;
        border-radius: 5px;
        opacity: 0; /* initially hidden */
        padding: 0.5rem;
        pointer-events: none;
      }

      .y-axis .domain {
        display: none;
      }
    </style>
  </head>
  <body></body>
</html>
```

### bar-chart.js with axes

```js
const BAR_HEIGHT = 20;
const BAR_MARGIN = 4; // vertical spacing between
const LEFT_PADDING = 70;
const PADDING = 20;
const SVG_WIDTH = 400;

const barTotal = BAR_HEIGHT + BAR_MARGIN;
const players = [
  {name: 'Mark', score: 20},
  {name: 'Tami', score: 40},
  {name: 'Amanda', score: 50},
  {name: 'Jeremy', score: 68},
  {name: 'Dasher', score: 12}
];
// Leave room for top PADDING, bottom PADDING, and x axis.
const svgHeight = PADDING * 3 + players.length * barTotal;
const usableHeight = svgHeight - PADDING * 3;
const usableWidth = SVG_WIDTH - LEFT_PADDING - PADDING;

// Get the highest score rounded up to the nearest multiple of 10.
const highScore = Math.ceil(d3.max(players, player => player.score) / 10) * 10;

// Create a linear scale that maps values from zero to the maximum score
// to values from zero to the width of the SVG.
const widthScale = d3
  .scaleLinear()
  .domain([0, highScore])
  .range([0, usableWidth]);

// Create an SVG element.
const svg = d3
  .select('body')
  .append('svg')
  .attr('width', SVG_WIDTH)
  .attr('height', svgHeight);

const tooltip = d3.select('body').append('div').classed('tooltip', true);

// Create a selection containing one SVG group for each data value
// that are translated in the y-direction so they are visually separated.
const barGroups = svg
  .selectAll('g')
  .data(players)
  .enter()
  .append('g')
  .classed('bar', true)
  .attr(
    'transform',
    (_, i) => `translate(${LEFT_PADDING}, ${PADDING + i * barTotal})`
  );

// Create a rect for each data value.
barGroups
  .append('rect')
  .attr('width', player => widthScale(player.score))
  .attr('height', BAR_HEIGHT)
  // Cannot use an arrow function because we need the value of "this".
  .on('mouseenter', function (player) {
    // Configure the tooltip.
    tooltip
      .text(player.score)
      .style('left', d3.event.pageX + 'px')
      .style('top', d3.event.pageY + 'px');
    // Show the tooltip.
    tooltip.transition().style('opacity', 1);
    // Fade the bar.
    d3.select(this).style('opacity', 0.5);
  })
  // Cannot use an arrow function because we need the value of "this".
  .on('mouseout', function () {
    // Hide the tooltip.
    tooltip.transition().style('opacity', 0);
    // Restore the bar opacity.
    d3.select(this).style('opacity', 1);
  });

// Create text for each data value that displays a player score.
barGroups
  .append('text')
  .text(player => player.score)
  .attr('x', player => widthScale(player.score) - 24) // at end of bar
  .attr('y', barTotal / 2 + 3); // centered vertically

const xAxisScale = d3
  .scaleLinear()
  .domain([0, highScore])
  .range([0, usableWidth]);
const xAxisMinor = d3
  .axisBottom(xAxisScale)
  .ticks(highScore) // show a tick at every 1
  .tickFormat('') // hides labels
  .tickSize(5); // length of each tick (default is 6)
const xAxisMajor = d3
  .axisBottom(xAxisScale)
  .ticks(highScore / 10) // show a tick at every multiple of 10
  // highStore is guaranteed to be a multiple of 10.
  .tickPadding(10) // space between end of tick and label; default is 3
  .tickSize(10);
//.tickSize(-usableHeight); // to draw across chart
const xAxisTransform = `translate(${LEFT_PADDING}, ${
  PADDING + players.length * barTotal
})`;
svg
  .append('g')
  .call(xAxisMinor)
  .classed('minor-x-axis', true)
  .attr('transform', xAxisTransform);
svg
  .append('g')
  .call(xAxisMajor)
  .classed('major-x-axis', true)
  .attr('transform', xAxisTransform);

// Generate tick values that will place the ticks
// at the vertical center of each of the bars.
const yTickValues = players.map((_, i) => i + 0.5);

const yAxisScale = d3
  .scaleLinear()
  .domain([players.length, 0]) // reversed order
  .range([usableHeight, 0]); // top to bottom
const yAxis = d3
  .axisLeft(yAxisScale)
  .ticks(players.length)
  .tickFormat((_, i) => {
    const player = players[i];
    return player ? player.name : '';
  })
  .tickValues(yTickValues);
svg
  .append('g')
  .call(yAxis)
  .classed('y-axis', true)
  .attr('transform', `translate(${LEFT_PADDING}, ${PADDING - BAR_MARGIN / 2})`);
```

## Pie Charts

![SVG pie chart](/blog/assets/d3-svg-pie-chart.png)

### index.html for pie chart

```html
<html>
  <head>
    <title>D3 SVG Pie Chart</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
    <style>
      body {
        display: flex;
        align-items: flex-start;

        font-family: sans-serif;
      }

      .key {
        display: inline-block;

        background-color: linen;
        margin-left: 1rem;
        width: 100px;
      }

      .player {
        display: flex;
        justify-content: space-between;

        color: white;
        padding: 0.5rem;
      }

      .tooltip {
        position: absolute;
        background: lightgray;
        border: solid gray 1px;
        border-radius: 5px;
        opacity: 0; /* initially hidden */
        padding: 0.5rem;
        pointer-events: none;
      }
    </style>
  </head>
  <body>
    <script src="pie-chart.js"></script>
  </body>
</html>
```

### pie-chart.js

```js
const players = [
  {name: 'Mark', score: 20},
  {name: 'Tami', score: 40},
  {name: 'Amanda', score: 50},
  {name: 'Jeremy', score: 68},
  {name: 'Dasher', score: 12},
  {name: 'Maisey', score: 37}
];

const totalScore = players.reduce((acc, player) => acc + player.score, 0);
console.log('pie-chart.js x: totalScore =', totalScore);

const size = 300;
const radius = size / 2;

const tooltip = d3.select('body').append('div').classed('tooltip', true);

const svg = d3
  .select('body')
  .append('svg')
  .attr('width', size)
  .attr('height', size);
const g = svg.append('g').attr('transform', `translate(${radius}, ${radius})`);

const color = d3.scaleOrdinal([
  'red',
  'orange',
  'yellow',
  'green',
  'blue',
  'purple'
]);

const fontColor = d3.scaleOrdinal([
  'white',
  'white',
  'black',
  'white',
  'white',
  'white'
]);

const pie = d3.pie().value(player => player.score);

const arc = d3.arc().innerRadius(0).outerRadius(radius);

const arcs = g
  .selectAll('arc')
  .data(pie(players))
  .enter()
  .append('g')
  .attr('class', 'arc');
arcs
  .append('path')
  .attr('fill', (_, i) => color(i))
  .attr('d', arc)
  .on('mousemove', function (data) {
    const player = data.data;
    console.log('pie-chart.js x: player =', player);
    // Configure the tooltip.
    tooltip
      .html(player.name + '<br>' + player.score)
      .style('left', d3.event.pageX + 'px')
      .style('top', d3.event.pageY + 'px');
    // Show the tooltip.
    tooltip.style('opacity', 1);
    // Fade the bar.
    d3.select(this).style('opacity', 0.5);
  })
  // Cannot use an arrow function because we need the value of "this".
  .on('mouseout', function () {
    // Hide the tooltip.
    tooltip.style('opacity', 0);
    // Restore the bar opacity.
    d3.select(this).style('opacity', 1);
    d3.select(this).style('outline', 'none');
  });

const playerDivs = d3
  .select('body')
  .append('div')
  .classed('key', true)
  .selectAll('div')
  .data(players)
  .enter()
  .append('div')
  .classed('player', true)
  .style('background-color', (_, i) => color(i))
  .style('color', (_, i) => fontColor(i));

playerDivs
  .append('div')
  .classed('name', true)
  .text(player => player.name);
playerDivs
  .append('div')
  .classed('score', true)
  .text(player => player.score);
```

## Loading Data

Discuss the `csv` and `json` methods.

## Geo

See <https://github.com/d3/d3-geo>.
