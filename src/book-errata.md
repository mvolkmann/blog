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

p. 32 Add the following after "Enter `npm install`
to install all the required dependencies from npm."

"To use TypeScript instead of JavaScript, enter `node scripts/setupTypeScript.js`.
This will modify the project to use TypeScript and delete the `scripts` directory."

p. 35 Add the following as a new second paragraph:

"To use TypeScript instead of JavaScript inside a `script` element,
change the opening tag to `<script lang="ts">`."

### Chapter 5

p. 87 Replace the first paragraph with the following:

"TypeScript can be used to define the types of props and
catch errors in passing props to components at compile time.
If TypeScript is not used, prop types can be checked at run time."

### Index

p. 419 Swap and correct the page numbers for
"action function" and "actions in Storybook:

action function 128-129

actions in Storybook 227-231
