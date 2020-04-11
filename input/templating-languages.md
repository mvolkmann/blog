---
layout: layout.njk
tags: navItem
title: Templating Languages
---

The default templating language available for use in Markdown files is Liquid.
Liquid was created by Shopify.
It is implemented in Ruby.

Another popular option is Nunjucks.
It was created by Mozilla and is implemented in JavaScript.
Nunjucks is very similar to Liquid.
Much of the syntax is identical.
But Nunjucks has many features that are not present in Liquid.

For details on Nunjucks syntax, see my notes file `NunjucksNotes.md` and
[Nunjucks Templating](https://mozilla.github.io/nunjucks/templating.html)
It's a pretty extensive language!

It is not necessary to install Nunjucks in order to use it in 11ty,
but it must be enabled.
To enable use of Nunjucks in a particular Markdown file,
add the following front matter:

```yaml
templateEngineOverride: njk,md
```

To enable use of Nunjucks in all Markdown files,
configure it in `.eleventy.js` using one of these approaches:

```js
module.exports = {
  markdownTemplateEngine: 'njk'
};
```

or

```js
module.exports = eleventyConfig => {
  // Call methods on eleventyConfig here.

  return {
    markdownTemplateEngine: 'njk'
  };
};
```

All the code examples that follow assume the use of Nunjucks.
