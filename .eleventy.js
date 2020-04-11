const inclusiveLangPlugin = require('@11ty/eleventy-plugin-inclusive-language');
const pwaPlugin = require('eleventy-plugin-pwa');

const itemHasTag = (item, tag) => item.data.tags.includes(tag);
const itemDoesNotHaveTag = (item, tag) => !item.data.tags.includes(tag);

module.exports = eleventyConfig => {
  eleventyConfig.addPlugin(inclusiveLangPlugin);
  eleventyConfig.addPlugin(pwaPlugin, {
    swDest: './_site/service-worker.js',
    globDirectory: './_site'
  });

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
