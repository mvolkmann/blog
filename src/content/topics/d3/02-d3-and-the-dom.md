---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  key: D3 and the DOM
  order: 2
  parent: D3
  title: D3 and the DOM
layout: topic-layout.njk
---

At its core, D3 provides some of the functionality of jQuery
for querying and manipulating the DOM.
If you have used jQuery, you are familiar with the idea that
the `$` function can be passed a CSS-like selector string and it
returns an object that encapsulates zero or more matching DOM elements.

D3 does something similar in its "Selections" module
which is included by default.
This adds many methods to the `d3` object.
It also defines the methods available on selection object.
Highlights of these methods are provided below.

For more detail, see the official documentation for the d3-selection module
at <https://github.com/d3/d3-selection/blob/v1.4.1/README.md>.

## Querying the DOM

### `d3.selectAll(selector)`

This method returns a "selection" that contains a
reference to every element that matches a given selector.
This is a empty collection if no matches are found.
We will talk much more about selections later,
but for now you think of them as being like a jQuery object.

For example, the following line finds
all elements that have the CSS class `bar`,
regardless of their nesting level in the document.

```js
const selection = d3.selectAll('.bar');
```

If you use `console.log` to output `selection` in the DevTools console,
you will see that it is an object that has a
`_groups` property that holds an array of groups.
In this case there is only one group.
The group is represented by a DOM `NodeList` that contains references to
the each of the DOM elements that have a class of "bar".

### `selection.selectAll(selector)`

This method is similar to `d3.selectAll(selector)`,
but only returns elements that are descendants of elements in the selection.

### `d3.select(selector)`

This method is similar to `selectAll`, but returns a selection
that only encapsulates the first matching DOM element.

### `selection.select(selector)`

This method is similar to `d3.select(selector)`,
but only returns the first matching element
that is a descendant of an element in the selection.

### `selection.filter(filter)`

This method creates a new selection that only contains
a subset of the elements in the selection.
The `filter` argument can be a selector string or a predicate function.
For example, `const reds = selection.filter('.red');` creates a selection
that only contains elements in `selection` with a class of `red`.
And `const small = bars.filter(d => d < 10);` creates a selection
that only contains elements with a bound data value that is less than 10.
TODO: BUT YOU HAVEN'T TALKED ABOUT DATA BINDING YET!

### `selection.merge(otherSelection)`

This method creates a new selection that combines
the selection on which it is called with a given selection.
For example, `const s3 = s1.merge(s2);` creates a selection
that contains all the elements in both `s1` and `s2`.

### `d3.style(node, cssPropertyName)`

This method returns the value of a CSS property on a given node.
For example, `const color = d3.style(node, 'color');`
returns the value of the CSS property `color` on `node`.

## Modifying the DOM

### `selection.attr(name [, value])`

When only passed an attribute name,
this returns the value of that attribute on the first element in the selection.
When passed both a name and a value,
this sets the attribute to the given value on every element in the selection.
The selection is returned to support call chaining.

### `selection.classed(classes [, boolean])`

When only passed a string of CSS class names, this returns a boolean
indicating whether the first element in the selection has those classes.
When passed both a string of CSS class names and a boolean value,
those classes are added to all the elements in the selection if the boolean is true,
and removed if it is false.
The selection is returned to support call chaining.

### `selection.style(name [, value])`

When only passed a CSS property name, this returns the current value
of that property in the first element of the selection.
When passed a CSS property name and a value,
this sets that property to the given value for all elements in the selection.
If the value is a function, it is invoked once for each element in the selection
and is passed the data bound to the element and the element index
so that a different value can be computed for each element.
The selection is returned to support call chaining.

### `selection.property(name, [, value])`

This is similar to `selection.attr(name [, value])`,
but operates on special properties that are not treated as attributes.
For example, it can be used on the `value` of a form element
or the `checked` value of a checkbox.

### `selection.text([value])`

When no value is passed, this returns the
text content of the first element in the selection.
When a value is passed, this sets the
text content of every element in the selection.
If the value is a function, it is invoked on each element in the selection,
is passed the data bound to the element and its index,
and returns the text content to be used for the element.
The selection is returned to support call chaining.

### `selection.html([value])`

This is similar to `selection.text([value])`,
but operates on the inner HTML instead of the text content.
The value is parsed as HTML, so it can include character entities
that specify Unicode characters, and other HTML strings.

### `selection.append(type)`

This appends a new element of the given type (ex. `'div'`)
as the last child of each element in the selection.
If it is passed a function instead of a string,
the function is invoked for each element in the selection,
is passed the data bound to the element and its index,
and returns the name of the new element to be appended.
This returns a new selection whose update selection contains
both the existing elements and the appended elements.
This is useful for modifying both sets of elements.

### `selection.insert(type [, before])`

This is similar to `selection.append(type)`,
but it inserts a new element before each element in the selection.
For detail on the `before` argument,
see the official documentation for the d3-selection module.
This returns a new selection containing the inserted elements.

### `selection.remove()`

This removes all element in the selection from the document.
It returns a new selection containing the removed elements.

### `selection.clone([deep])`

This inserts a clone of each element in the selection
immediately after each of those elements.
If `deep` is specified and is truthy, descendant elements are also cloned.
Otherwise, references to the existing descendant elements
are used in the clones.

### `selection.sort(comparator)`

This reorders the elements in the selection within the DOM and
returns a new selection that contains the elements in the selection
sorted based on the comparator function.
The comparator function follows the same rules as comparator functions
that are used with the JavaScript `Array` `sort` method.

### `selection.raise()`

This moves each element in the selection to be the last child of its parent.

### `selection.lower()`

This moves each element in the selection to be the first child of its parent.

### `d3.create(name)`

This creates an element with the given name and
returns a selector containing this element.
It is not yet attached to any other element.

## Joining Data

### `selection.data([dataArray [, keyFunction]])`

This binds the values in `dataArray` to the elements in the selection.
It is a fundamental part of understanding D3.
There are three possibilities to consider:

1. The number of values in `dataArray`
   matches the number of elements in `selection`,
   so no elements need to be created or removed.
1. There are more values in `dataArray`
   than there are elements in `selection`,
   so more elements need to be created.
1. There are fewer values in `dataArray`
   than there are elements in `selection`,
   so some existing elements need to be removed.

The `data` method returns a new selection that contains the properties
`_groups` (the "update" selection),
`_enter` (the "enter" selection), and
`_exit` (the "exit" selection).
Each of these sub-selections is an array of arrays where typically
the outer array holds a single array.

From <https://bost.ocks.org/mike/selection/>,
"Selections returned by `d3.select` and `d3.selectAll` have exactly one group.
The only way for you to obtain a selection with multiple groups
is `selection.selectAll`." and
"With selectAll, every element in the old selection
becomes a group in the new selection;
each group contains an old element’s matching descendant elements."

For example:

```js
const tds = d3.selectAll('tr').selectAll('td');
```

The variable `tds` is set to a selection object whose `_groups` property
is set to an array with one member for each `tr`.
Those members are arrays that hold each of the `td` elements
in the corresponding `tr`.

The update selection contains elements that were
already present in the selection.
The enter selection contains placeholders for elements
that need to be created.
The exit selection contains elements for which no data was bound.
The elements in the update and enter selections
have a `__data__` property that holds its bound value.

For example, suppose `selection` contains three elements
and `dataArray` contains five values.
In the returned selection,
the update selection will contain the three existing elements,
the enter selection will contain two placeholders,
and the exit selection will be empty.
The enter selection can be used to
append an element for each of the placeholders.

As another example, suppose `selection` contains five elements
and `dataArray` contains three values.
In the returned selection,
the update selection will contain the first three existing elements,
the enter selection will be empty,
and the exit selection will contain the two elements
for which no data value could be bound.
The exit selection can be used to remove the excess elements.

If a `keyFunction` is not provided, data from `dataArray`
is bound to the elements in `selection` in the order in which it appears.
If a `keyFunction` is provided,

### `selection.join(enter [, update [, exit]])`

This provides an alternative to using the `enter` and `exit` methods
that many find easier to understand.
The arguments are all functions that invoked to handle
adding new elements (enter),
updating existing elements (update),
and removing excess elements (exit).
If no exit function is provided,
excess elements are removed by default,
which is typically the desired behavior.

The fruit bar chart example demonstrates using the `join` method.

### selection.enter

### selection.exit

### selection.datum

## Handling Events

### selection.on

### selection.dispatch

### d3.event

### d3.customEvent

### d3.mouse

### d3.touch

### d3.touches

### d3.clientPoint

## Control Flow

### selection.each

### selection.call

### selection.empty

### selection.nodes

### selection.node

### selection.size

## Local Variables

### d3.local

### local.set

### local.get

### local.remove

### local.toString

---

TODO: Do you want anything below this point?

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
