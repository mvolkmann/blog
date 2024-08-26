---
eleventyNavigation:
  key: Transpiling
  order: 60
  parent: Smalltalk
layout: topic-layout.njk
---

There have been efforts to write Smalltalk code that transpiles
other Smalltalk code into code written in another programming language.
At a high level, the steps to do this are:

- Walk the source AST generated from the Smalltalk code to be transpiled.
- Generate a new AST for the target language.
- Infer types to be specified in the target language code.
- Optimize the target AST.
- Walk the target AST and generate target source code.
