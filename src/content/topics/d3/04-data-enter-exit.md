---
css: '/blog/assets/github-extensions.css'
eleventyNavigation:
  key: D3 data/enter/exit
  order: 4
  parent: D3
  title: data, enter, & exit
layout: topic-layout.njk
---

## data method

The data method "joins" data values in an array to DOM elements.
By default it does this by index.
The first data array value is joined to the first target DOM element, and so on.

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
          .selectAll('li') // A - see B below
          .data(letters)
          .text(d => d);

        lis
          .enter()
          .append('li') // B - should match element at A above
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

In the example above, we call `selectAll` with "li"
and also call `append` with "li".
The important thing is that we select elements that match those we will create.
Another option is to select elements with certain CSS class,
perhaps using `'.my-class'`, and create elements that have that class,
perhaps using `.append('div').attr('class', 'my-class')`.

The `data` method can be passed a second argument that is a function.
This takes a data value and returns
a string that uniquely identifies the data value.
This provides an alternative to joining data values to DOM elements by index.
When this approach is used, changes to data that is already joined
to a DOM element result in the same DOM elements being updated
and their order within their parent element is maintained.

Here is an example that demonstrates this:

```html
<html>
  <head>
    <title>D3 enter/exit demo</title>
    <script src="https://d3js.org/d3.v5.min.js"></script>
  </head>
  <body>
    <ol></ol>

    <button onclick="addOne()">Add One</button>
    <button onclick="removeFirst()">Remove First</button>
    <button onclick="removeLast()">Remove Last</button>

    <script>
      const people = [
        {name: 'Mark', color: 'yellow'},
        {name: 'Tami', color: 'blue'},
        {name: 'Amanda', color: 'purple'},
        {name: 'Jeremy', color: 'black'}
      ];

      const toString = person => person.name + ' likes ' + person.color;

      function populateList() {
        const lis = d3
          .select('ol')
          .selectAll('li')

          // With no key function, this uses the existing li elements.
          //.data(people)

          // With a key function, this doesn't use
          // existing li elements because keys don't match.
          // The person argument is undefined when the empty
          // li elements are processed in the first call.
          .data(people, person => (person ? person.name : ''))

          .text(toString);

        lis
          .enter()
          .append('li')
          .text(person => toString(person).toUpperCase());

        lis.exit().remove();
      }

      function addOne() {
        people.push({name: 'Joe', color: 'brown'});
        populateList();
      }

      function removeFirst() {
        people.shift();
        // Passing a key function to the data method above
        // allows the exiting li elements that match data
        // to retain their position in the list despite this sort.
        // If the key function is not passed, the list will be reordered
        // to match this sorted order.
        people.sort((p1, p2) => p1.name.localeCompare(p2.name));
        console.log('enter-exit-people.html removeFirst: people =', people);
        populateList();
      }

      function removeLast() {
        people.pop();
        populateList();
      }

      populateList();
    </script>
  </body>
</html>
```

For another explanation of the `data`, `enter`, and `exit` methods,
see <https://medium.com/@c_behrens/enter-update-exit-6cafc6014c36>,

For a detailed explanation of the internals, see Mike Bostock's article
"How Selections Work" at <https://bost.ocks.org/mike/selection/>.
