---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  key: D3 data/entry/exit
  order: 2
  parent: D3
  title: data, entry, & exit
layout: topic-layout.njk
---

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
