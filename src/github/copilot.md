---
eleventyNavigation:
  key: GitHub Copilot
  parent: GitHub
  title: Copilot
layout: topic-layout.njk
---

This post summarizes ways to interact with GitHub Copilot in VS Code.

## Q&A Blocks

In any source file, enter the syntax for a comment
followed by "q:" and a question, and press the return key.
For example, in a JavaScript file you could enter

```js
// q: What does REST stand for?
```

That will display something like:

```js
// a: Representational State Transfer
```

To accept the answer and add that line to the file, press the tab key.

To see other possible answers, hover over the answer to expose a toolbar.

## Chat Windows

Press ctrl-shift-i in Windows or cmd-shift-i in macOS
to open a text input at the top where a question can be entered.
This option is better for questions that have long answers.

You can rate the answer by clicking the thumbs up or thumbs down icons.

To close the answer dialog, press the escape key
or click the "X" in the upper-right.

## Side Window

Click the speech bubbles icon in the left nav bar.
This opens a new left panel where a question can be entered
in a text input at the bottom.

This panel maintains a list of all previously asked questions
and the answer provided, so you can scroll back and review them.

## Code Suggestions

In a source file, enter a comment requesting a function that
performs some operation. Copilot will respond with comments that
describe what the function must do, followed by a suggested implementation.
Press the tab key after each line is output to accept it
and proceed to the next line.

For example, in JavaScript we could enter the following:

```js
// Write a function that calculates the median of an array of numbers.
```

Copilot will respond with lines like the following:

```js
// The median is the number that is halfway into the sorted array.
// If there is an odd number of values, the median is the middle value.
// If there is an even number of values, the median is the average of the two middle values.
// You can use the sort method to sort the array.
// You can use Math.floor to round down.
// You can use the length property to help compute the index.
function median(values: number[]) {
  values.sort((a, b) => a - b);
  const half = Math.floor(values.length / 2);
  if (values.length % 2 === 0) {
    return (values[half - 1] + values[half]) / 2;
  }
  return values[half];
}
```
