---
layout: layout.njk
title: 11ty Configuration
---

Configuration options for 11ty are documented
[here](https://www.11ty.dev/docs/config/).

By default the file `.eleventy.js` is used
to specify configuration options for 11ty.
This file makes its default export
either an object or a function.
When it is a function, it has the structure shown below.
When it is an object, it is like the return value of this function.

This example configuration explicitly
sets each option to its the default value.

```js
module.exports = eleventyConfig => {
  //TODO: Add examples of adding filters, shortcodes, custom tags,
  //TODO: JavaScript functions, custom collections, and plugins.

  eleventyConfig.addLinter('linter-name', (content, inputPath, outputPath => {
    // Add code to lint template output
    // in "content" that came from "inputPath".
    // Error messages can include "inputPath".
    // Presumably if anything is fixed,
    // updated content should be written to "outputPath".
  });

  // Copies files in a given directory to output directory
  // without performing any processing on them.
  eleventyConfig.addPassthroughCopy('assets');

  eleventyConfig.addTransform('transform-name', (content, outputPath => {
    // Add code to transform template output in content
    // and write the result to outputPath.
    // For example, this can be used to format or minify output HTML.
  });

  // Rebuilds the site if any files in a watch target directory change.
  //TODO: What directories are watched by default?
  //TODO: Probably the data, includes, input, and layouts directories.
  // This is an example, not the default value.
  eleventyConfig.addWatchTarget("./scss/");

  // Overrides BrowserSync default configuration options.
  // None are overridden by default.
  eleventyConfig.setBrowserSyncConfig({ options });

  // Performs deep merge of data from sources in data cascade.
  // This currently defaults to false, but may change to true soon.
  eleventyConfig.setDataDeepMerge(false);

  // Override default configuration options of the gray-matter package
  // which is used to parse front matter.
  // See https://github.com/jonschlinkert/gray-matter.
  eleventyConfig.setFrontMatterParsingOptions({
    options
  });

  // Suppresses output of the paths of all generated files.
  eleventyConfig.setQuietMode(false);

  // Watches all JavaScript templates and data files
  // and rebuilds the site if they change.
  eleventyConfig.setWatchJavaScriptDependencies(true);

  return {
    dataTemplateEngine: 'liquid', // used in global data files
    dir: { // specifies the directory for various kinds of files
      data: '_data', // for global data files
      includes: '_includes', // for layouts and more
      input: '.',
      layouts: // defaults to the includes value
      output: '_site'
    },
    htmlOutputSuffix: '-o', // rarely useful
    htmlTemplateEngine: 'liquid', // used in HTML files
    jsDataFileSuffix: '.11tydata', // for template & directory data files
    // No processing is allowed in JSON files,
    // so no template engine can be specified.
    markdownTemplateEngine: 'liquid', // used in Markdown files
    pathPrefix: '/', // prepended to all URL paths
    templateFormats: [
      '11ty.js', 'ejb', 'haml', 'hbs', 'html',
      'liquid', 'md', 'mustache', 'njk', 'pug'
    ]
  };
};
```

TODO: Why goes in the "includes" directory besides layout files?
ANSWER: include files, extends files, partials, and macros
TODO: But what are these?
