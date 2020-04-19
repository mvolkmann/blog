const htmlmin = require('html-minifier');
//const inclusiveLangPlugin = require('@11ty/eleventy-plugin-inclusive-language');
const navigationPlugin = require('@11ty/eleventy-navigation');
const syntaxHighlightPlugin = require('@11ty/eleventy-plugin-syntaxhighlight');
const fs = require('fs');

//const itemHasTag = (item, tag) => item.data.tags.includes(tag);
//const itemDoesNotHaveTag = (item, tag) => !item.data.tags.includes(tag);

/**
This recursively sorts an array of documents.
The primary sort is on the "order" property.
The secondary sort is on title.
*/
function sortDocuments(arr) {
  if (!arr) return;

  arr.sort((doc1, doc2) => {
    const order1 = doc1.order;
    const order2 = doc2.order;
    if (order1) {
      return order2 ? order1 - order2 : -1;
    } else if (order2) {
      return -1;
    } else {
      return doc1.title.localeCompare(doc2.title);
    }
  });

  arr.forEach(doc => sortDocuments(doc.children));
}

module.exports = eleventyConfig => {
  // This filters page objects based on a data property value.
  eleventyConfig.addFilter('filter', (arr, property, value) => {
    return arr.filter(obj => obj.data[property] === value);
  });

  // This filters page objects based on a data property value.
  eleventyConfig.addFilter('filterNot', (arr, property, value) => {
    return arr.filter(obj => obj.data[property] !== value);
  });

  eleventyConfig.addFilter('hasOrder', arr => {
    return arr.filter(obj => Boolean(obj.data.eleventyNavigation.order));
  });

  // This filter is being added in v0.11.0.
  eleventyConfig.addFilter('log', value => {
    console.log('.eleventy.js log: value =', value);
    return value;
  });

  eleventyConfig.addFilter('noOrder', arr => {
    return arr.filter(obj => !obj.data.eleventyNavigation.order);
  });

  eleventyConfig.addFilter('navSort', arr => {
    sortDocuments(arr);
    return arr;
  });

  //eleventyConfig.addPlugin(inclusiveLangPlugin);

  // Copies files in a given directory to output directory
  // without performing any processing on them.
  eleventyConfig.addPassthroughCopy('src/assets');

  eleventyConfig.addPassthroughCopy('src/service-worker.js');

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

  eleventyConfig.addWatchTarget('_site/assets/*.css');

  eleventyConfig.setBrowserSyncConfig({
    files: ['_site/assets/*.css']
  });

  // Suppresses output of the paths of all generated files.
  eleventyConfig.setQuietMode(true);

  // Watches all JavaScript templates and data files
  // and rebuilds the site if they change.
  //eleventyConfig.setWatchJavaScriptDependencies(true);

  // Create JSON file that is read by service-worker.js.
  try {
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
  } catch (e) {
    // Don't treat it as an error if the _site/assets directory doesn't exist.
    if (e.code !== 'ENOENT') throw e;
  }

  return {
    dir: {input: 'src'},
    markdownTemplateEngine: 'njk', // used in Markdown files
    pathPrefix: '/blog/', // prepended to all URL paths
    templateFormats: ['11ty.js', 'html', 'md', 'njk']
  };
};
