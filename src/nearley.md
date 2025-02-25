---
eleventyNavigation:
  key: Nearley
layout: topic-layout.njk
---

<style>
  img {
    border: 1px solid gray;
  }
</style>

## Overview

{% aTargetBlank "https://nearley.js.org", "nearley" %}
is a parsing toolkit with many features including:

- Handles all BNF grammars, including those with left recursion.
- Can produce abstract syntax trees, text output, or simply validate input.
- Provides a testing tool (`nearley-test`).
- Provides a railroad diagram generator that creates HTML files
  that include SVG-based diagrams (`nearley-railroad`).
- Works with many lexers including
  {% aTargetBlank "https://github.com/no-context/moo", "Moo" %}.

## Installing

To install nearley in a node project, enter `npm install nearley`.
This also installs the Moo lexer library.

## First Grammar

## ASTs
