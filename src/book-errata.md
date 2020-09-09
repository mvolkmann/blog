---
eleventyNavigation:
  key: Book Errata
layout: topic-layout.njk
---

This is an errata for the book
"[Svelte and Sapper in Action](https://www.manning.com/books/svelte-and-sapper-in-action)".
If you find additional issues, please email them to
<a href="mailto:r.mark.volkmann@gmail.com?subject=Svelte book errata" target="_blank">me</a>.

### Chapter 1

p. 11 Change the second paragraph in section 1.1.3 to:

"Svelte is implemented in TypeScript, but until July 21, 2020 it did not
support using TypeScript to define components out of the box.
This could be achieved with a little configuration and added tooling.
But now opting into the use of TypeScript is much easier.
See https://svelte.dev/blog/svelte-and-typescript for details."

p. 11 Replace "It" at the start of the the second sentence in the note with:

"The official TypeScript support added in July, 2020 uses this. Chapter 20"

p. 15 The following paragraph belongs with the bullet that precedes it:

"Sites created this way are not required to be entirely static.
They can still include JavaScript code and
take advantage of Svelte reactivity for updating the DOM."

### Chapter 2

p. 29 In the first bullet of section 2.1.6,
change "next section" to "section 2.1.8".

p. 31 In section 2.2.1 at the end of the paragraph about using Webpack,
"app-name" should be in italic.

p. 32 Add the following after "Enter `npm install`
to install all the required dependencies from npm."

"To use TypeScript instead of JavaScript, enter `node scripts/setupTypeScript.js`.
This will modify the project to use TypeScript and delete the `scripts` directory."

p. 32 The sentence that starts with "Note that the default Webpack"
belongs with step 5 above it.

p. 35 Add the following as a new second paragraph:

"To use TypeScript instead of JavaScript inside a `script` element,
change the opening tag to `<script lang="ts">`."

p. 36 In step 4, change "listing 2.4" to "listing 2.5".

p. 36 In the paragraph that starts with "The TodoList",
add a space between "TodoList" and "component".

p. 36 In the last sentence of the same paragraph,
change "component" to "components".

### Chapter 3

p. 45 In the last paragraph, change "this component" to "the Person component".

p. 48 In section 3.4 in the paragraph that starts with "The following example",
change "always" to "only".

p. 54 In section 3.9 near the end of the first paragraph,
change "these" to "interpolations".

p. 56 In the first sentence, "average" should be in code font.

p. 58 Change the title of Listing 3.5 to
"Using module context in src/Demo.svelte".

p. 58 In listing 3.6, add the following line after the opening script tag:

import {onMount} from 'svelte';

p. 58 In the call to "console.log" in listing 3.6,
remove "home.svelte on Mount: ".

p. 59 In the second paragraph of section 3.12, "Todo" should be in code font.

### Chapter 5

p. 85 In the paragraph that begins with "The value of Boolean props",
change "listing 5.4" to "listing 5.5".

p. 87 Replace the first paragraph with the following:

"TypeScript can be used to define the types of props and
catch errors in passing props to components at compile time.
If TypeScript is not used, prop types can be checked at run time."

p. 93 Remove two spaces of indentation
from the line "\$: triple = cValue \* 3;".

p. 103 Steps 1 and 2 for Checklist.svelte are not really needed
until before step 1 for Checklist.svelte on p. 104.

### Chapter 6

p. 106 Change the description of "Readable stores" to
"do not allow components to change their data"
because the store itself can change its own data.

p. 112 The "key" attribute on the "option" element is not needed.

### Chapter 7

p. 125 The closing brace after "{@html expression" can be on the same line
and the book intended to show it that way.

### Chapter 8

p. 146 Change "Here is the Color component" to "Here is the ColorCycle"
component for consistency with the previous version on p. 145.

p. 146 In the title of Listing 8.6, change "Color" to "ColorCycle".

p. 146 In Listing 8.6, add the following after the import statement
to match code in Listing 8.4:

export let text

p. 146 In Listing 8.6, change "Some Title" to "{text}"
to match code in Listing 8.4.

p. 149 Code indentation in Listing 8.9 is wrong.
All indentation should use two spaces like this:

```js
import {onMount} from 'svelte';

export function onMountFocus() {
  onMount(() => {
    const input = document.querySelector('input'); <1>
    input.focus();
  });
}

export function onMountLog(name) {
  onMount(() => console.log(name, 'mounted'));
}
```

p. 150 In the sentence that starts with "The following listing",
add a space between "AgeEntry" and "or".

### Chapter 9

p. 156 The closing brace of the else block near the top of the page
is indented one space too far.

p. 157 The tr element inside tfoot should have an empty td inserted between
the two currently there to leave an empty cell in the Quantity column.

p. 157 The style tag and its content should be removed
since it isn't doing anything.

p. 160 In Listing 9.9 at the bottom of the page, the content of the h1
should be "Page Not Found".

### Chapter 12

p. 202 Add the following to the end of the annotation in Listing 12.4
that begins with "This waits":

It returns a Promise.

p. 204 In the second to last annotation in Listing 12.6,
change "fond" to "found".

p. 208 Near the end of the test output shown in Listing 12.11,
change the indentation of the numbers after "Tests:", "Snapshots:",
and "Time:" to match that of "Test Suites:".

p. 220 in the second paragraph, change all letters in color hex codes
to lowercase for consistency.

p. 223 Add a new first step in the list after the sentence that starts with
"To run WAVE" that is "Browse to the website to be tested."

p. 226 At the end of the bullet "It adds the following npm scripts",
add "in package.json" with "package.json" in code font.

### Chapter 13

p. 240 In the second sentence, change "Create-React-App" to "Create React App"
to match the Vercel website.

### Chapter 14

p. 245 Delete the first comment in Listing 14.1 which just duplicates
the annotation on the following line.

p. 245 Indent the <label> element inside the {#each} block by two spaces.

p. 250 In the first sentence of section 14.3,
add a space between "name" and "props".

p. 250 In the last sentence, change "manual" to "hash".

p. 251 The NOTE should be indented so it appears to
belong with the second bullet.

p. 252 In the sentence that begins with "Add the following",
delete the period before "src/Checklist.svelte".

p. 253 Indent the NOTE so it appears to belong with
the paragraph that precedes it.

p. 256 In Listing 14.4, change "options={options}" to "{options}".

p. 259 Indent the Note so it appears to belong with step #5
that begins on the previous page.

p. 259 Step numbering should not have restarted.
Change step 1 to step 6.
Change step 2 to step 7.
Change step 3 to step 8.

p. 259 Indent the gray box so it appears to belong with what is now step #6.

p. 259 The last two paragraphs should be combined into one paragraph
because they address the same subject.

### Chapter 16

p. 279 In Listing 16.3, line 3, delete ", preloading, session"
because those variables are not used.

p. 287 Fix the indentation of the code in step 13.
All but the first line are indented two spaces too far.

### Chapter 17

p. 292 In Figure 17.1, change "Ramsey" to "Ramsay".

p. 292 In the first bullet after the line "With this UI we can",
change "a database" to "the database".

p. 294 In the second annotation, change the last word from "dogs" to "dogMap".

p. 296 In the two button elements near the top of the page, change
on:click|preventDefault={saveDog}
to
on:click={saveDog} type="button"

p. 296 Delete the annotation that says
"Using preventDefault prevents a form submission."
because setting the button types to "button" removes the need for this.

p. 301 In step 7, change "routes/checklist.svelte"
to "components/Category.svelte".

p. 301 In the list under the bullet for "src directory"
all the indentation was lost. It should be:

```text
- `routes` directory
  - `categories` directory
    - `index.json.js`—`get` and `post` category
    - `[categoryId]` directory
      - `index.json.js`—`put` and `del` category
      - `items` directory
        - `index.json.js`—`post` item
        - `[itemId].json.js`—`put` and `del` item
```

p. 306 In second paragraph, change "that would be" to "that is".

### Chapter 18

p. 316 In code near the top of the page, change

{% raw %}

<div class="left">
to
<div>
{% endraw %}

### Chapter 19

p. 323 In the paragraph that begins with "Service workers do not",
change "Worker-postMessage" (split across lines) to "Worker postMessage",
removing the dash. These are separate words.

p. 325 For consistency with other code comments,
near the bottom of the page change
{% raw %}
...use online in JavaScript code and {#if} blocks...
to
// use online in JavaScript code and {#if} blocks
{% endraw %}

p. 327 In section 19.4, step 1, change "cache" to code font.

p. 333 Indent the NOTE so it appears to belong to step 3.

p. 333 Step numbers should not reset to 1 after the NOTE.
Change 1 to 4.

p. 334
Change 2 to 5.
Change 3 to 6.
Change 4 to 7.
Change 5 to 8.

### Chapter 20

p. 344 In the first paragraph of section 20.1, add a space between
"String" and "replace".

### Index

p. 419 Swap and correct the page numbers for
"action function" and "actions in Storybook:

action function 128-129

actions in Storybook 227-231
