const inclusiveLangPlugin = require('@11ty/eleventy-plugin-inclusive-language');
const navigationPlugin = require('@11ty/eleventy-navigation');
const pwaPlugin = require('eleventy-plugin-pwa');
const syntaxHighlightPlugin = require('@11ty/eleventy-plugin-syntaxhighlight');

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

  // I commented this out during testing so all files
  // are loaded from the network instead of the cache.
  // See https://github.com/okitavera/eleventy-plugin-pwa/issues/5.
  /*
  eleventyConfig.addPlugin(pwaPlugin, {
    cleanupOutdatedCaches: true,
    swDest: './_site/service-worker.js',
    globDirectory: './_site'
    //mode: 'production'
  });
  */

  // Copies files in a given directory to output directory
  // without performing any processing on them.
  eleventyConfig.addPassthroughCopy('input/assets');

  eleventyConfig.addPlugin(navigationPlugin);

  eleventyConfig.addPlugin(syntaxHighlightPlugin);

  // Suppresses output of the paths of all generated files.
  //eleventyConfig.setQuietMode(false);

  // Watches all JavaScript templates and data files
  // and rebuilds the site if they change.
  //eleventyConfig.setWatchJavaScriptDependencies(true);

  return {
    dir: {
      input: 'input'
    },
    markdownTemplateEngine: 'njk', // used in Markdown files
    pathPrefix: '/blog/', // prepended to all URL paths
    templateFormats: ['11ty.js', 'html', 'md', 'njk']
  };
};
