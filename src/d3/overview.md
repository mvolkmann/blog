---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  key: D3 Overview
  parent: D3
  title: Overview
layout: topic-layout.njk
---

D3 stands for Data-Driven Documents.
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

## Basic Bar Charts

![basic SVG bar chart](/blog/assets/d3-svg-bar-chart.png)

### index.html

```html
<html>
  <head>
    <title>D3 SVG Bar Chart</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
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
  <body>
    <script src="barchart.js"></script>
  </body>
</html>
```

### barchart.js

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
They are

## Loading Data

Discuss the `csv` and `json` methods.

## Geo

See <https://github.com/d3/d3-geo>.
