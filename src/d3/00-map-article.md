---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  parent: D3
  title: D3 Maps SETT Article
layout: topic-layout.njk
---

## Introduction

The previous article found here covers the basics of using D3
and shows how to create a fully-featured bar chart.
This article expands on that knowledge to render geographic maps.

## SVG Paths

SVG `path` and `polygon` elements can be used to draw
the outline of a geographic area such as a country, state, or city.

The `path` element supports a large number of attributes,
the most commonly used are the following:

- `d` - a string of space-separated commands that describe what will be drawn
- `fill` - the color to be used to fill the shape
- `stroke` - the line color
- `stroke-width` - the line width

There are many commands that can be specified in the value of the `d` attribute.
Each command name is a single letter followed by one or more numbers.
Uppercase commands specify absolute locations.
Lowercase commands specify relative locations.

The M command moves to a given x and y location.
The h command draws a horizontal line with a given dx.
The v command draws a vertical line with a given dy.
The l command draws a line with a given dx and dy.

By default the origin is the upper-left corner.
The coordinate system can be flipped so the origin is in the lower-left corner
by placing the SVG elements inside a group (`g`) element like the following
that uses the `translate` and `scale` functions:

```html
<g transform="translate(0 12) scale(1 -1)"></g>
```

## Latitude and Longitude

A handy web site for finding the approximate latitude and longitude
of any point on the earth can be found at <http://teczno.com/squares>.
Simple zoom in/out and drag the map to location a spot of interest
and note the reported latitude and longitude values.

SEE YOUR Development/D3 bookmarks in Chrome!

See http://mjmdavis.com/showing/2017/05/16/how-to-read-maps.html
See https://bl.ocks.org/mbostock/3711652
See https://source.opennews.org/articles/choosing-right-map-projection/

## 3D to 2D Projections

## Finding Map Data

We need two kinds of map data.
One kind provides SVG path outlines of countries, states, and cities.
Another kind provides data about geographic regions.
Examples in include population and number of corona virus cases.

See https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html.
See http://www.naturalearthdata.com/

## Using Data for Colors

## Using Data for Tooltips

## Adding Drag and Zoom

### A generic approach

### The D3-specific approach
