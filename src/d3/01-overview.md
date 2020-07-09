---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  key: D3 Overview
  order: 1
  parent: D3
  title: Overview
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
