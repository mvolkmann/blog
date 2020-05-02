---
eleventyNavigation:
  key: Minify HTML
  parent: Eleventy
layout: topic-layout.njk
---

To minify the HTML produced for each page,
install html-minifier by entering the following:

```bash
npm install -D html-minifier
```

Then add the following in `.eleventy.js`:

```js
module.exports = eleventyConfig => {
  eleventyConfig.addTransform('htmlmin', (content, outputPath) => {
    if (!outputPath || !outputPath.endsWith('.html')) return content;
    return htmlmin.minify(content, {
      useShortDoctype: true,
      removeComments: true,
      collapseWhitespace: true
    });
  });
};
```
