---
eleventyNavigation:
  key: GitHub Copilot
  parent: GitHub
  title: Copilot
layout: topic-layout.njk
---

This post summarizes ways to interact with GitHub Copilot in VS Code.

## Status

To see the status of GitHub Copilot, click the button in the lower-right.

<img alt="GitHub Copilot status button" style="width: 10%"
  src="/blog/assets/github-copilot-status-button.png?v={{pkg.version}}">

Clicking this button displays a menu of options
that begins with the current status.

<img alt="GitHub Copilot status menu" style="width: 70%"
  src="/blog/assets/github-copilot-status-menu.png?v={{pkg.version}}">

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

<img alt="GitHub Copilot chat window" style="width: 80%"
  src="/blog/assets/github-copilot-chat-window.png?v={{pkg.version}}">

You can rate the answer by clicking the thumbs up or thumbs down icons.

To close the answer dialog, press the escape key
or click the "X" in the upper-right.

## Side Window

Click the speech bubbles icon in the left nav bar.
This opens a new left panel where a question can be entered
in a text input at the bottom.

This panel maintains a list of all previously asked questions
and the answer provided, so you can scroll back and review them.

<img alt="GitHub Copilot side window" style="width: 40%"
  src="/blog/assets/github-copilot-side-window.png?v={{pkg.version}}">

## Code Suggestions

In a source file, enter a comment requesting a function that
performs some operation. Copilot will respond with comments that
describe what the function must do, followed by a suggested implementation.
Press the tab key after each line is output to accept it
and press the return key to proceed to the next line.

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

Sometimes there is more than one code suggestion.
Hover over the code to display a toolbar where alternative code suggestions
can be displayed by clicking the "<" and ">" buttons.

Press ctrl-return to see all the code suggestions in a new split pane.
Click one of the "Accept Solution" links above each code suggestion
to add that code to the source file.

## Explain It

To get an explanation of an entire source file,
right-click and select "Copilot > Explain This".

To get an explanation of a specific function implementation,
select all of its lines, right-click, and select "Copilot > Explain This".

The GitHub Copilot side window will open, if not already open,
and the explanation will appear there.

## Fix It

To get suggestions for fixing syntax errors,
select the lines to be examined, right-click, and select "Copilot > Fix This".

## Generating Documentation

To generate documentation for a function or section of code,
select the lines to be examined, right-click,
and select "Copilot > Generate Docs".

For example, the following documentation is generated for an "average" function.

```js
/**
 * Calculates the average of an array of numbers.
 * @param values - The array of numbers.
 * @returns The average of the numbers.
 */
function average(values: number[]) {
  return values.reduce((sum, value) => sum + value, 0) / values.length;
}
```

## Generating Tests

To generate tests for a function,
move the cursor to the first line of a function implementation, right-click,
and select "Copilot > Generate Tests".

For example, the following function is in the file `math.js`.

```js
export function avg(numbers) {
  const sum = numbers.reduce((a, b) => a + b, 0);
  return sum / numbers.length;
}
```

Copilot will create the file `math.test.js` containing the following.
Note that it is missing imports for `test` and `expect`.

```js
import {avg} from './math';

test('calculates the average of an array of numbers', () => {
  expect(avg([1, 2, 3, 4, 5])).toBe(3);
});

test('calculates the average of an array with negative numbers', () => {
  expect(avg([-1, -2, -3, -4, -5])).toBe(-3);
});

test('calculates the average of an array with decimal numbers', () => {
  expect(avg([1.5, 2.5, 3.5, 4.5, 5.5])).toBe(3.5);
});

test('calculates the average of an empty array', () => {
  expect(avg([])).toBe(NaN);
});
```

## Explain in Terminal

When code that is run in a terminal produces error messages,
clicking the "two stars" icon on the left and selecting "Explain using Copilot"
describes the cause of the errors and sometimes suggested code fixes.
Press ctrl-return to insert the code suggestions where the cursor is located.
Manually delete the incorrect lines.

## Automatic Commit Messages

After making changes to files in a Git repository,
click the "Source Control" button in the left nav and
click the "two starts" icon in the text input for the commit message.
Copilot will suggest a commit message
that can be edited before committing the changes.

<img alt="GitHub Copilot automatic commit message" style="width: 100%"
  src="/blog/assets/github-copilot-automatic-commit-message.png?v={{pkg.version}}">

## Alternatives

See {% aTargetBlank
"https://bito.ai/blog/free-github-copilot-alternatives-for-vs-code/",
"10 Free GitHub Copilot Alternatives for VS Code 2023" %}.
