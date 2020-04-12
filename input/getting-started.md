---
eleventyNavigation:
  key: Getting Started
  order: 4
intro: true
layout: layout.njk
tags: [navItem, intro]
title: Getting Started
---

Here are the most basic steps to create and build an 11ty site.

1. Create a new directory and cd to it.
1. Enter `npm init -y`.
1. Enter `npm install -D @11ty/eleventy`.
1. Create the file `index.md` file and add any Markdown content.
1. Enter `npx eleventy --serve`
1. Browse localhost:8080 to see the site.

This automatically watches the current directory for file changes.
When they are detected, it rebuilds the site and
attempts to refresh the browser (using BrowserSync).
NOTE: BrowserSync does not currently work with 11ty!
See <https://github.com/11ty/eleventy/issues/701>.

If the project is placed in a Git repository,
add `_site/` to the `.gitignore` file
since that only contains generated code.

Optionally add these npm scripts in `package.json`
that can be used to build and serve the site:

```json
"build": "eleventy",
"clean": "rm -rf _site",
"start": "eleventy --serve"
```

To deploy an 11ty site to GitHub Pages,
see [here](/blog/github-pages).
