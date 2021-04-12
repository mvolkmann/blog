---
eleventyNavigation:
  key: Authentication
layout: topic-layout.njk
---

This page describes an approach for implementing
web-based authentication from scratch, not using libraries.
This is useful for understanding all the underlying steps
or implementing a custom authentication approach.

To use ESM, edit `package.json` and add the line `"type": "module",`.
This enables using the following syntax to
import a JavaScript file from another one:

```js
import './some-name.js'; // file extension is required
```
