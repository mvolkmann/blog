module.exports = eleventyConfig => {
  // Create a custom collection of sorted nav items.
  eleventyConfig.addCollection('navItemsSorted', collection =>
    collection
      .getFilteredByTag('navItem')
      .sort((item1, item2) => item1.data.title.localeCompare(item2.data.title))
  );

  // Copies files in a given directory to output directory
  // without performing any processing on them.
  eleventyConfig.addPassthroughCopy('assets');

  // Suppresses output of the paths of all generated files.
  //eleventyConfig.setQuietMode(false);

  // Watches all JavaScript templates and data files
  // and rebuilds the site if they change.
  //eleventyConfig.setWatchJavaScriptDependencies(true);

  return {
    markdownTemplateEngine: 'njk', // used in Markdown files
    templateFormats: ['11ty.js', 'html', 'md', 'njk']
  };
};
