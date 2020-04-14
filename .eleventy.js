const htmlmin = require('html-minifier');
const inclusiveLangPlugin = require('@11ty/eleventy-plugin-inclusive-language');
const navigationPlugin = require('@11ty/eleventy-navigation');
const syntaxHighlightPlugin = require('@11ty/eleventy-plugin-syntaxhighlight');
const fs = require('fs');

//const itemHasTag = (item, tag) => item.data.tags.includes(tag);
//const itemDoesNotHaveTag = (item, tag) => !item.data.tags.includes(tag);

module.exports = eleventyConfig => {
  // This filters page objects based on a data property value.
  eleventyConfig.addFilter('filter', (arr, property, value) => {
    return arr.filter(obj => obj.data[property] === value);
  });

  // This filters page objects based on a data property value.
  eleventyConfig.addFilter('filterNot', (arr, property, value) => {
    return arr.filter(obj => obj.data[property] !== value);
  });

  // This filter is being added in v0.11.0.
  eleventyConfig.addFilter('log', value => {
    console.log('.eleventy.js log: value =', value);
    return value;
  });

  eleventyConfig.addPlugin(inclusiveLangPlugin);

  // Copies files in a given directory to output directory
  // without performing any processing on them.
  eleventyConfig.addPassthroughCopy('input/assets');

  eleventyConfig.addPassthroughCopy('input/service-worker.js');

  eleventyConfig.addPlugin(navigationPlugin);

  eleventyConfig.addPlugin(syntaxHighlightPlugin);

  // Minify generated HTML.
  eleventyConfig.addTransform('htmlmin', (content, outputPath) => {
    if (!outputPath || !outputPath.endsWith('.html')) return content;
    return htmlmin.minify(content, {
      useShortDoctype: true,
      removeComments: true,
      collapseWhitespace: true
    });
  });

  // Suppresses output of the paths of all generated files.
  eleventyConfig.setQuietMode(true);

  // Watches all JavaScript templates and data files
  // and rebuilds the site if they change.
  //eleventyConfig.setWatchJavaScriptDependencies(true);

  // Create JSON file that is read by service-worker.js.
  let files = fs.readdirSync('_site/assets');
  files = files.map(file => '/blog/assets/' + file);
  files.push('/blog/'); // cache the start URL (Lighthouse wants this)
  const timestamp = Date.now();
  const serviceWorkerData = {files, timestamp};
  fs.writeFileSync(
    '_site/service-worker-data.json',
    JSON.stringify(serviceWorkerData)
  );
  console.log('wrote service-worker-data.json with timestamp', timestamp);

  return {
    dir: {
      input: 'input'
    },
    markdownTemplateEngine: 'njk', // used in Markdown files
    pathPrefix: '/blog/', // prepended to all URL paths
    templateFormats: ['11ty.js', 'html', 'md', 'njk']
  };
};
