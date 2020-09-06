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

### Index

p. 419 Swap and correct the page numbers for
"action function" and "actions in Storybook:

action function 128-129

actions in Storybook 227-231
