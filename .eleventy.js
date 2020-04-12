const inclusiveLangPlugin = require('@11ty/eleventy-plugin-inclusive-language');
const navigationPlugin = require('@11ty/eleventy-navigation');
const pwaPlugin = require('eleventy-plugin-pwa');
const syntaxHighlightPlugin = require('@11ty/eleventy-plugin-syntaxhighlight');

const itemHasTag = (item, tag) => item.data.tags.includes(tag);
const itemDoesNotHaveTag = (item, tag) => !item.data.tags.includes(tag);

module.exports = eleventyConfig => {
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

  eleventyConfig.addFilter('filter', (arr, property, value) => {
    return arr.filter(obj => obj.data[property] === value);
  });

  // This filter is being added in v0.11.0.
  eleventyConfig.addFilter('log', value => {
    console.log('.eleventy.js log: value =', value);
    return value;
  });

  //TODO: Does this filter already exist in Nunjucks?
  eleventyConfig.addFilter('sort', (arr, property) => {
    arr.sort((obj1, obj2) => obj1[property].localeCompare(obj2[property]));
    return arr;
  });

  eleventyConfig.addPlugin(navigationPlugin);

  eleventyConfig.addPlugin(syntaxHighlightPlugin);

  // Create a custom collection of sorted, intro nav items.
  eleventyConfig.addCollection('introNavItemsSorted', collection =>
    collection
      .getFilteredByTag('navItem')
      .filter(item => itemHasTag(item, 'intro'))
      .sort((item1, item2) => item1.data.order - item2.data.order)
  );

  // Create a custom collection of sorted, non-intro nav items.
  eleventyConfig.addCollection('advancedNavItemsSorted', collection =>
    collection
      .getFilteredByTag('navItem')
      .filter(item => itemDoesNotHaveTag(item, 'intro'))
      .sort((item1, item2) => item1.data.title.localeCompare(item2.data.title))
  );

  // Copies files in a given directory to output directory
  // without performing any processing on them.
  eleventyConfig.addPassthroughCopy('input/assets');

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
