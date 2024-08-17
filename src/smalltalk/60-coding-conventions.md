---
eleventyNavigation:
  key: Coding Conventions
  order: 60
  parent: Smalltalk
layout: topic-layout.njk
---

- Should there be a space after the return operator ^ ?
  Jon says "no".

- Should there be spaces inside the square brackets of a block?
  Jon says "no".

- Should there be spaces around the vertical bar (|) that
  separates parameters from code in a block?
  Jon says "yes".

- Should there be spaces around `@` when creating a `Point`?
  Jon says "yes".

- Should the keywords of long keyword messages
  be on separate lines, indented from the receiver on the first line?

- Should the last statement in a method or block be terminated with a period
  even though it isn't required?
  Jon says "yes" if the method contains more than one statement.

- Should there be a blank like after the line that declares local variables?
  Jon says "no", but he includes a blank line before it.

- Should the closing `]` of a multiline block be on a new line?
  Jon says "no". He places it at the end of the last statement.
