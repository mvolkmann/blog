const htmlmin = require('html-minifier');
//const inclusiveLangPlugin = require('@11ty/eleventy-plugin-inclusive-language');
const navigationPlugin = require('@11ty/eleventy-navigation');
const syntaxHighlightPlugin = require('@11ty/eleventy-plugin-syntaxhighlight');
const markdownItAnchor = require('markdown-it-anchor');
const tocPlugin = require('eleventy-plugin-nesting-toc');
const pkg = require('./package.json');

/*
// Configure use of Katex for rendering math equations.
const markdownIt = require('markdown-it');
const markdownItKatex = require('markdown-it-katex');
const options = {breaks: false, html: true, linkify: true};
//const markdownLib = markdownIt(options).use(markdownItKatex);
const markdownLib = markdownIt(options);
markdownLib.use(markdownItKatex);
*/

// Configure use of MathJAX for rendering math equations.
const markdownIt = require('markdown-it');
const options = {breaks: false, html: true, linkify: true};
const mathJax = require('markdown-it-mathjax');
const sups = require('markdown-it-sup');
const subs = require('markdown-it-sub');
const footnotes = require('markdown-it-footnote');
const markdownLib = markdownIt(options)
  .use(mathJax()) // https://github.com/classeur/markdown-it-mathjax shows need for ()
  .use(subs)
  .use(sups)
  .use(footnotes)
  .use(markdownItAnchor);

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
      return 1;
    } else {
      return doc1.title.localeCompare(doc2.title);
    }
  });

  arr.forEach(doc => sortDocuments(doc.children));
}

module.exports = eleventyConfig => {
  eleventyConfig.setLibrary('md', markdownLib);

  eleventyConfig.addCollection('nav', collection => {
    const keyMap = {};
    const navMap = {};

    // Create an array of item objects that contain
    // "eleventyNavigation" front matter.
    const navItems = collection.items.filter(
      item => item.data.eleventyNavigation
    );

    // Create a mapping from item keys to objects that describe them.
    for (const item of navItems) {
      const {data, url} = item;
      const {eleventyNavigation: nav} = data;
      const {key, title} = nav;

      if (!title) nav.title = key;
      //TODO: pkg is not defined!
      if (url) nav.url = `/blog${url}?v=${pkg.version}`;

      keyMap[key] = nav;
    }

    // Turn the items into a tree structure based on parent relationships.
    for (const obj of Object.values(keyMap)) {
      const {parent} = obj;
      if (parent) {
        parentObj = keyMap[parent];
        if (parentObj) {
          let {children} = parentObj;
          if (!children) children = parentObj.children = [];
          children.push(obj);
        }
      } else {
        navMap[obj.key] = obj;
      }
    }

    // Get an array of the top-level items.
    const navColl = Object.values(navMap);

    // Sort the items within their parent.
    sortDocuments(navColl);

    return navColl;
  });

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
    //console.log('.eleventy.js log: value =', value);
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
  eleventyConfig.addPassthroughCopy('src/js');

  eleventyConfig.addPlugin(navigationPlugin);
  eleventyConfig.addPlugin(tocPlugin);
  eleventyConfig.addPlugin(syntaxHighlightPlugin);

  eleventyConfig.addShortcode(
    'aInternal',
    (url, text) => `<a href="${url}" target="frame">${text}</a>`
  );

  eleventyConfig.addShortcode(
    'aTargetBlank',
    (url, text) =>
      `<a href="${url}?v=${pkg.version}" rel="noopener" target="_blank">${text}</a>`
  );

  eleventyConfig.addShortcode(
    'aTargetBlankNoVersion',
    (url, text) => `<a href="${url}" rel="noopener" target="_blank">${text}</a>`
  );

  // Minify generated HTML.
  eleventyConfig.addTransform('htmlmin', (content, outputPath) => {
    if (!outputPath || !outputPath.endsWith('.html')) return content;
    return htmlmin.minify(content, {
      useShortDoctype: true,
      removeComments: true,
      collapseWhitespace: true
    });
  });

  eleventyConfig.addWatchTarget('_includes/*.js');
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
  /*
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
  */

  return {
    dir: {input: 'src'},
    markdownTemplateEngine: 'njk', // used in Markdown files
    pathPrefix: '/blog/', // prepended to all URL paths
    templateFormats: ['11ty.js', 'html', 'md', 'njk']
  };
};
