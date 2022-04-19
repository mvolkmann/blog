---
eleventyNavigation:
  key: IE11 Support
  parent: Svelte
layout: topic-layout.njk
---

Polyfills are required in order for a Svelte app to run in IE11.
Here are the steps required:

1. Add the following in `public/index.html`
   before the `script` tag for `bundle.js`:

   ```html
   <script defer src="https://polyfill.io/v3/polyfill.min.js"></script>
   ```

1. Add the following in `rollup.config.js` just before the line `production && terser()`:

   ```js
   babel({
     extensions: ['.js', '.mjs', '.html', '.svelte'],
     exclude: ['node_modules/@babel/**', 'node_modules/core-js/**'],
     presets: [
       [
         '@babel/preset-env',
         {
           targets: {
             ie: '11'
           },
           useBuiltIns: 'usage',
           corejs: 3
         }
       ]
     ],
     plugins: [
       '@babel/plugin-syntax-dynamic-import',
       [
         '@babel/plugin-transform-runtime',
         {
           useESModules: true
         }
       ]
     ],
     runtimeHelpers: true
   }),
   ```

1. Enter `npm install -D` for each of these packages:

   - @babel/plugin-syntax-dynamic-import
   - @babel/plugin-transform-runtime
   - @babel/runtime
   - @rollup/plugin-babel
   - corejs
   - whatwg-fetch

1. In each source file where the Fetch API is being used, add the following:

   ```js
   import 'whatwg-fetch';
   ```

1. Remove all use of CSS variables.

1. Test all uses of CSS flexbox and grid layout.
   There are some bugs in the IE11 implementation of these.

Unfortunately all the polyfills make the generated `bundle.js` file much bigger.
For one app the size changed from 52K to 410K.
