---
layout: layout.njk
title: Syntax Highlighting
---

Install the plugin.

```bash
npm install -D @11ty/eleventy-plugin-syntaxhighlight
```

Modify .eleventy.js:

```js
const syntaxHighlight = require('@11ty/eleventy-plugin-syntaxhighlight');
module.exports = eleventyConfig => {
  ...
  eleventyConfig.addPlugin(syntaxHighlight);
};
```

Download a Prism theme `.css` file from
[Prism themes](https://github.com/PrismJS/prism-themes)
and place it in the `assets` directory.
For example, `prism-vs.css`.

Add a `link` tag for this in all layout files whose pages need it.
For example, this can be added in `_includes/layout.njk`.

```html
<link rel="stylesheet" href="/assets/prism-vs.css" />
```

Surround code to have syntax highlighting with fences.
For example:

````text
 ```js
 code goes here
 ```
````

For more details, see the documentation [here](https://www.11ty.dev/docs/plugins/syntaxhighlight/).

There is also a way to trigger syntax highlighting
with Nunjucks, but some editors like VS Code
will change the indentation in code blocks
when using this approach.
