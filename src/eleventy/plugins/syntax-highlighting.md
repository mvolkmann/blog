---
eleventyNavigation:
  key: Syntax Highlighting
  parent: Plugins
layout: topic-layout.njk
---

To use the syntax highlighting plugin, install it as follows:

```bash
npm install -D @11ty/eleventy-plugin-syntaxhighlight
```

Then register it in `.eleventy.js` as follows:

```js
const syntaxHighlightPlugin = require('@11ty/eleventy-plugin-syntaxhighlight');

module.exports = eleventyConfig => {
  eleventyConfig.addPlugin(syntaxHighlightPlugin);
};
```

Download a Prism theme `.css` file from {% aTargetBlank
  'https://github.com/PrismJS/prism-themes/',
  'Prism themes'
%}.

and place it in the `assets` directory.
For example, `prism-ghcolors.css`.
More themes can be found at {% aTargetBlank
  'https://prismjs.com/',
  'prismjs.com'
%}.

Add a `link` tag for this in all layout files whose pages need it.
For example, this can be added in `_includes/layout.njk`.

```html
<link rel="stylesheet" href="/assets/prism-vs.css" />
```

Nunjucks is not a supported PrismJS language,
but Liquid is supported and seems good enough to use with Nunjucks syntax.
So instead of putting "njk" after three backticks, use "liquid".

Surround code to have syntax highlighting with fences.
For example:

````text
 ```js
 code goes here
 ```
````

For more details, see the documentation {% aTargetBlank
  'https://www.11ty.dev/docs/plugins/syntaxhighlight/',
  'here'
%}.

There is also a way to trigger syntax highlighting
with Nunjucks, but some editors like VS Code
will change the indentation in code blocks
when using this approach.

It appears that showing line numbers is not supported yet.
See {% aTargetBlank
  'https://github.com/11ty/eleventy-plugin-syntaxhighlight/issues/10',
  'line numbers issue'
%}.
