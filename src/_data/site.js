const eleventyPackage = require('@11ty/eleventy/package.json');

module.exports = () => {
  const {name, version} = eleventyPackage;
  return {generator: `${name} v${version}`};
};
