---
eleventyNavigation:
  key: Styling with Sass
  parent: Styling
layout: layout.njk
---

Sass provides a modified CSS syntax that can be compiled to plain CSS.
Sass provides many benefits over CSS including
more compact variable assignment and usage,
nested rules, mixins, and provided functions.
For more details, see the [Sass home page](https://sass-lang.com/).

One option for adding the use of Sass to an 11ty project is to use
[eleventy-plugin-sass](https://www.npmjs.com/package/eleventy-plugin-sass).
However, there are currently several issues with this plugin.

Another way we can make converting `.scss` files to `.css` files
an automated part of the 11ty build process is to follow these steps:

1. Place all `.scss` files in the `assets` directory.
1. Enter `npm install -D node-sass npm-run-all`.
1. Modify `package.json` to contain the following npm scripts.
   This assumes that the input directory configured in `.eleventy.js`
   is named `input`.

```json
"build": "npm run sass && eleventy",
"sass": "node-sass -o _site/assets --output-style compact input/sass",
"serve": "npm-run-all sass --parallel watch:*",
"watch:eleventy": "eleventy --serve",
"watch:sass": "npm run sass -- --watch"
```

Add the following in `.eleventy.js` so the browser will refresh
when node-sass creates or updates `.css` files:

```js
eleventyConfig.addWatchTarget('_site/assets/*.css');

eleventyConfig.setBrowserSyncConfig({
  files: ['_site/assets/*.css']
});
```

Now when `npm run build` is used to build the site
or when `npm serve` is used to serve and watch the site,
`.scss` files in the `assets` directory will be
compiled to `.css` files in the `_site/assets` directory.
Changes made to `.scss` files while `npm serve` is running
will automatically be compiled to `.css` files
and the browser will reload to show the results.

The reason `.scss` files are not placed under the `assets` directory
is that `eleventyConfig.addPassthroughCopy` is typically used to
copy the contents of the `assets` directory to `_site`.
We do not want to copy `.scss` files there
because they are not needed by the running site.
