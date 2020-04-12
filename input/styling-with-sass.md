---
eleventyNavigation:
  key: Styling with Sass
layout: layout.njk
title: Styling with Sass
---

Sass provides a modified CSS syntax that can be compiled to plain CSS.
Sass provides many benefits over CSS including
more compact variable assignment and usage,
nested rules, mixins, and provided functions.
For more details, see the [Sass home page](https://sass-lang.com/).

We can make converting `.scss` files to `.css` files
an automated part of the 11ty build process
by following these steps:

1. Create a `sass` directory at the top of the project
   and place all `.scss` in it.
1. Enter `npm install -D node-sass npm-run-all`.
1. Modify `package.json` to contain the following npm scripts:

```json
"build": "npm run sass && eleventy",
"sass": "node-sass sass --output assets/css",
"start": "npm-run-all sass --parallel watch:*",
"watch:eleventy": "eleventy --serve",
"watch:sass": "npm run sass -- --watch"
```

This assumes that all `.css` files will be
stored in the `assets/css` directory.
Now when `npm run build` is used to build the site
or when `npm start` is used to serve and watch the site,
`.scss` files in the `sass` directory will be
compiled to `.css` files in the `assets/css` directory.
Changes made to `.scss` files while `npm start` is running
will automatically be compiled to `.css` files
and the browser will reload to show the results.

The reason `.scss` files are not placed under the `assets` directory
is that `eleventyConfig.addPassthroughCopy` is being used to
copy the contents of the `assets` directory to `_site`.
We do not want to copy `.scss` files there
because they are not needed by the running site.

Optionally include the `--quiet` option on the `eleventy` commands.
