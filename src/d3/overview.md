---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  key: D3 Overview
  order: 1
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
// replaces value of class attribute
<code>.attr('class', '<i>className</i>')</code>
or
// updates value of class attribute
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
